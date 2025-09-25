(defmodule session
  (doc "REST hello world handler.")
  (export
    (init 2)
    (player 3)
    (waiting-player 2)
    (full-state->player-state 2)
    (available-actions 1)
    (serialize-game 2)
    (room 1)
    (initial-room 0))
  (export (allowed_methods 2)
          (handler 2)
          (content_types_accepted 2))
  (module-alias (collections coll)))

(defun init (req opts)
  "Switch to the REST protocol and start executing the state machine."
  `#(cowboy_rest ,req ,opts))

(defun allowed_methods (req state)
  `#([#"GET" #"HEAD" #"OPTIONS" #"POST"] ,req ,state))

(defun content_types_accepted (req state)
  `#([#(#"application/xml" handler)] ,req ,state))

(defun public-information ()
  '(discard-pile open-hand))

(defun extract-if-self (player-number)
  (lambda (player index)
    (if (== index player-number)
      player
      (maps:with (public-information) player))))

(defun full-state->player-state (state player-number)
  (let ((players (coll:tmap
                   (extract-if-self player-number)
                   (mref state 'players))))
    (mset (maps:with '(current-player) state)
      'players players
      'you player-number)))

(defun serialize-single-open-hand (list-melds)
  (lists:map
   (lambda (t)
     (let (((tuple tag list-tiles) t))
       (xmerl:export_simple_element (tuple tag '() (lists:map (fun tiles:serialize 1) list-tiles)) 'xmerl_xml)))
   list-melds))

(defun serialize-open-hand (list-melds)
  (tuple 'open-hand
	 (xmerl:export_simple_element
	  (tuple 'open-hand '() (serialize-single-open-hand list-melds)) 'xmerl_xml)))

(defun serialize-pile (tag pile)
  (tuple tag
	 (xmerl:export_simple_element
	  (tuple tag '() (lists:map (fun tiles:serialize 1) pile)) 'xmerl_xml)))

(defun yaku-han-entry->tuple
  (((tuple yaku quantity))
   (xmerl:export_simple_element (tuple yaku (list (tuple 'han quantity)) '()) 'xmerl_xml)))

(defun serialize-yaku-han (yaku-han)
  (xmerl:export_simple_element
   (tuple 'yakus '() (clj:->> yaku-han (maps:to_list) (lists:map (fun yaku-han-entry->tuple 1)))) 'xmerl_xml))

(defun serialize-full-player
  (((map 'hand hand 'discard-pile discard-pile 'open-hand open-hand 'yaku-han yaku-han 'stick-deposit stick-deposit))
     (tuple 'self
	    (xmerl:export_simple_element
	     (tuple 'self '() 		  
		    (list
		     (serialize-pile 'hand (coll:mset->list hand))
		     (serialize-pile 'discard-pile discard-pile)
		     (serialize-open-hand open-hand)
		     (serialize-yaku-han yaku-han)
		     (xmerl:export_simple_element (tuple 'stick-deposit (list (tuple 'quantity (erlang:integer_to_list stick-deposit))) '()) 'xmerl_xml)
		     )) 'xmerl_xml))))

(defun serialize-player (player-number)
  (lambda (player index)
    (xmerl:export_simple_element
     (tuple 'player
	    (if (== index player-number)
	      (list (serialize-full-player player))
	      (lists:foldl
	       (lambda (info acc)
		 (let ((value (map-get player info)))
		   (case info
		     ('discard-pile (cons (serialize-pile 'discard-pile value) acc))
		     ('open-hand (cons (serialize-open-hand value) acc)))))
	       (list)
	       (public-information))) '()) 'xmerl_xml)))

(defun serialize-game (player-number player-state)
  (let* ((players (map-get player-state 'players))
	 (serialized-players (clj:->> players (coll:tmap (serialize-player player-number)) (tuple_to_list)))
	 (xml-players (xmerl:export_simple_element (tuple 'players '() serialized-players) 'xmerl_xml))
	 (current-player (map-get player-state 'current-player)))
    (xmerl:export_simple (list (tuple 'game (list (tuple 'you (erlang:integer_to_list player-number))
						  (tuple 'current-player (erlang:integer_to_list current-player))
						  (tuple 'players xml-players)) '())) 'xmerl_xml)))

(defun available-actions (player-state)
  (lists:flatmap
    (lambda (f)
      (funcall f player-state))
    (list
      ;; TODO: add more checks
      ;; TODO: abstract common pattern
      (lambda (player-state)
        (if (yaku:call-riichi?
              (coll:get-in player-state
              (list 'players (mref player-state 'you) 'hand)))
          (list 'call-riichi)
          (list))))))

;; state: hand[2-man, 3-man, 1-pin, ...], last-command['discard], last-player[1], whoami[2]
;; returns: ['can-call-chi]
(defun player (state number sse-pid)
  (receive
    (`#(new-state ,state)
     (let ((visible-state (full-state->player-state state number)))
       ;; TODO: push this to the frontend via SSE
       (available-actions visible-state)
       (! sse-pid (tuple 'ok (tuple number visible-state)))
     (player state number sse-pid)))
    (unknown
     (io:format "Unknown message: ~p\n" (list unknown))
     (player state number sse-pid))))

(defun waiting-player (number sse-pid)
  (receive
    (`#(ready ,another-player-number)
     (! sse-pid
       (tuple 'info
         (io_lib:format "Player ~p is ready!" (list another-player-number))))
     (waiting-player number sse-pid))
    ('end (! sse-pid 'end))
    (`#(all-ready! ,initial-state)
     (io:format "Everything set! Ready, go!\n" (list))
     (player initial-state number sse-pid))))

(defun initial-room ()
  (map
    'players '()))

;; state :: {players :: { number :: int; pid :: bigint; ready? :: bool } list;
;;           decider-pid :: bigint}
;; TODO: This does not take into account people disconnecting or closing the browser.
;; We need to save more state (likely via mnesia) to recover from those situations.
(defun room (state)
  (receive
    (`#(connect ,sse-pid)
     (let* ((players (map-get state 'players))
	    (players-count (erlang:length players))
	    (player-number (+ players-count 1)))
       (io:format "Somebody connected: ~p\n" (list player-number))
       (if (< players-count 4)
	 (progn
	   (! sse-pid `#(connected ,player-number))
	   (room (coll:update-in state '(players)
			 (cons (tuple player-number
				      (spawn 'session 'waiting-player `(,player-number ,sse-pid))
				      'false)  players))))
	 (progn
	   (! sse-pid `#(error "Cannot enter: room is full"))
	   (room state)))))
    (`#(ready ,http-id ,player-id)
     ;; TODO: There should be a check for connect before a ready is signaled
     ;; TODO: Check for 4 players
     ;; Otherwise, someone may attempt to do a raw curl with ready without connecting first
     (io:format "Ready: player ~p.\n" (list player-id))
     (lists:foreach (lambda (player)
        (let ((waiting-player-process (tref player 2)))
          (! waiting-player-process `#(ready ,player-id))))
      (map-get state 'players))
     (! http-id 'ok)
     ;; updates the ready status of each player,
     ;; which is the third value in the tuple
     (io:format "Current room: ~p\n" (list state))
     (let* ((players (mref state 'players))
            (player-position (+ 1 ; silly 1-based indexing
                               (- (length players) player-id))))
       ;; TODO: get rid of the lists.
       (room (coll:update-in state `(players ,player-position 3) 'true))))
    (`#(terminate ,http-id ,player-id)
     (let* ((`#(,owner ,_ ,_) (lists:last (coll:get-in state '(players)))))
       (if (== player-id owner)
         (progn
           (lists:foreach
             (lambda (player)
               (let ((waiting-player-process (tref player 2)))
                 (! waiting-player-process `end)))
             (map-get state 'players))
           (! http-id 'ok)
           (room (initial-room)))
         (progn
           (! http-id (tuple 'error "You do not own the room."))
           (room state)))))
    (`#(start ,http-id ,player-id)
     ;; TODO: Same as other cases, handle the illegal state machine transitions
     (io:format "Somebody wants to start: ~p\n" (list player-id))
     (let* ((`#(,owner ,_ ,_) (coll:get-in state '(players 4)))
            (players (map-get state 'players))
            (ready-players (lists:filter (lambda (player) (== 'true (coll:get-in player '(3)))) players))
            (ready-players-count (erlang:length ready-players)))
       (io:format "Got inside the let.\nReady players: ~p\nOwner: ~p\nID: ~p\n"
         (list ready-players-count owner player-id))
       (if (and (== ready-players-count 4) (== owner player-id))
         (let* ((players-pids (clj:->> players (lists:reverse) (lists:map (lambda (player) (tref player 2)))))
                (initial-game-state (game:initial-game players-pids))
                (decider-id (spawn 'game 'decider (list initial-game-state))))
           (lists:foreach (lambda (player)
                            (let ((waiting-player-process (tref player 2)))
                              (! waiting-player-process `#(all-ready! initial-game-state))))
                          (map-get state 'players))
           (io:format "This is the initial game: ~p\n" (list initial-game-state))
           (! http-id 'ok)
           (room (map-set state 'decider-id decider-id)))
         (! http-id `#(error "Either not all players are ready or someone started without being owner of the room")))))
    (`#(play ,raw-params ,http-id ,player-id)
     ;; TODO: Same situation as ready with connected
     ;; Error handling on this for malicious intent
     (let* ((params (xml:play-action-params raw-params)))
        (io:format "Somebody wants to play: ~p\n~p\n" (list player-id params))
        (! (map-get state 'decider-id) params)
	(! http-id 'ok)
	(room state)))))

(defun handler (req state)
  (let* ((room-pid (mref state 'room))
         (body (clj:-> (cowboy_req:read_body req)
                       (tref 2)
                       (xml:one-element))))
    (case body
      (`#(terminate #m(player-id ,id))
       (! room-pid (tuple 'terminate (self) (list_to_integer id)))
       (receive
         ('ok `#(true ,req ,state))
         (`#(error ,_)
          ;; TODO: use the error message
          `#(false ,req ,state))))
      (`#(ready #m(player-id ,id)) ; TODO: read ID from the cookie
       (! room-pid (tuple 'ready (self) (list_to_integer id)))
       (receive ('ok `#(true ,req ,state))))
      (`#(start #m(player-id ,id)) ; TODO: read ID from the cookie
       (! room-pid (tuple 'start (self) (list_to_integer id)))
       (receive
         ('ok `#(true ,req ,state))
         (`#(error ,msg)
          (io:format "Errored out: ~p\n" (list msg))
          `#(false ,req ,state))))
      (`#(play ,(= raw-params `#m(player-id ,id)))
       (! room-pid (tuple 'play raw-params (self) (list_to_integer id)))
       (receive ('ok `#(true ,req ,state)))))))
