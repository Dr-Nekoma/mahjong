(defmodule session
  (doc "REST hello world handler.")
  (export
    (init 2)
    (player 2)
    (waiting-player 1)
    (full-state->player-state 2)
    (available-actions 1)
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
(defun player (state number)
  (receive
    (`#(new-state ,state)
     (let ((visible-state (full-state->player-state state number)))
       ;; TODO: push this to the frontend via SSE
       (available-actions visible-state))
     (io:format "Player ~p is ready to send via SSE!\n" (list number))
     (player state number))
    (unknown
     (io:format "Unknown message: ~p\n" (list unknown))
     (player state number))))

(defun waiting-player (number)
  (receive
    (`#(ready? ,another-player-number)
     (io:format "Player ~p is ready!\n" (list another-player-number))
     ;; TODO: We need to send a message to FE to inform that
     )
    (`#(all-ready! ,initial-state)
     (io:format "Everything set! Ready, go!\n" (list))
     (player initial-state number))))

(defun initial-room ()
  (map
    'players '()))

;; state :: {players :: { number :: int; pid :: bigint; ready? :: bool } list;
;;           decider-pid :: bigint}
;; TODO: This does not take into account people disconnecting or closing the browser.
;; We need to save more state (likely via mnesia) to recover from those situations.
(defun room (state)
  (receive
    (`#(connect ,http-id)
     (let* ((players (map-get state 'players))
	    (players-count (erlang:length players))
	    (player-number (+ players-count 1)))
       (io:format "Somebody connected: ~p\n" (list player-number))
       (if (< players-count 4)
	 (progn
	   (! http-id `#(ok ,player-number))
	   (room (coll:update-in state '(players)
			 (cons (tuple player-number
				      (spawn 'session 'waiting-player `(,player-number))
				      'false)  players))))
	 (progn
	   (! http-id `#(error room-is-full))
	   (room state)))))
    (`#(ready ,http-id ,player-id)
     ;; TODO: There should be a check for connect before a ready is signaled
     ;; TODO: Check for 4 players
     ;; Otherwise, someone may attempt to do a raw curl with ready without connecting first
     (progn
       (io:format "Somebody is ready: ~p\n" (list player-id))
       (! http-id 'ok)
       ;; updates the ready status of each player,
       ;; which is the third value in the tuple
       (room (coll:update-in state `(players ,player-id 3) 'true))))
    (`#(start ,http-id ,player-id)
     ;; TODO: Same as other cases, handle the illegal state machine transitions
     (io:format "Somebody wants to start: ~p\n" (list player-id))
     (let* ((`#(,owner ,_ ,_) (coll:get-in state '(players 4)))
	    (players (map-get state 'players))
       	    (ready-players (lists:filter (lambda (player) (== 'true (coll:get-in player '(3)))) players))
	    (ready-players-count (erlang:length ready-players)))
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
      (`#(connect ,_)
       (! room-pid (tuple 'connect (self)))
       (receive
         (`#(ok ,player-number)
          `#(true ,req ,state))
         (`#(error ,_)
          `#(false ,req ,state))))
      (`#(ready #m(player-id ,id)) ; TODO: read ID from the cookie
       (! room-pid (tuple 'ready (self) (list_to_integer id)))
       (receive ('ok `#(true ,req ,state))))
      (`#(start #m(player-id ,id)) ; TODO: read ID from the cookie
       (! room-pid (tuple 'start (self) (list_to_integer id)))
       (receive
         ('ok `#(true ,req ,state))
	 (`#(error ,_) `#(false ,req ,state))))
      (`#(play ,(= raw-params `#m(player-id ,id)))
       (! room-pid (tuple 'play raw-params (self) (list_to_integer id)))
       (receive ('ok `#(true ,req ,state)))))))
