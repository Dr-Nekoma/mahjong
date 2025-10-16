(defmodule room
  (export
    (initial 0)
    (run 1))
  (module-alias (collections coll)))

(defrecord room-player number pid ready?)

(defun initial ()
  (map 'players (map)))

(defun broadcast-msg (state msg)
  (clj:->> 'players
           (mref state)
           (maps:values)
           (lists:foreach
            (lambda (player)
              (let ((player-process (room-player-pid player)))
                (! player-process msg))))))

(defun broadcast (state event msg)
  (broadcast-msg state (tuple event msg)))

(defun broadcast (state event)
  (broadcast-msg state event))

(defun first-empty-spot (state)
  (let ((players (mref state 'players)))
    (case (lists:search (lambda (number) (not (maps:is_key number players))) '(1 2 3 4))
      ((tuple 'value key) (tuple 'ok key))
      ('false 'error))))

;; TODO: This does not take into account people disconnecting or closing the browser.
;; We need to save more state (likely via mnesia) to recover from those situations.
(defun run (state)
  (receive
    (`#(connect ,sse-pid)
     (case (first-empty-spot state)
       ((tuple 'ok player-number)
        (io:format "Somebody connected: ~p\n" (list player-number))
        (let ((new-state (coll:update-in
                          state
                          `(players ,player-number)
                          (record room-player
                                  number player-number
                                  pid (spawn 'session 'waiting-player `(,player-number ,sse-pid))
                                  ready? 'false))))
          (broadcast new-state 'info (io_lib:format "Player ~p connected!" (list player-number)))
          (run new-state)))
       ('error
        (! sse-pid (tuple 'error "Cannot enter: room is full"))
        (run state))))
    (`#(ready ,http-id ,player-id)
     (io:format "Ready: player ~p.\n" (list player-id))
     (let ((players (mref state 'players)))
       (case (coll:mref-safe players player-id)
         ((tuple 'ok player)
          (if (room-player-ready? player)
            (! (room-player-pid player) (tuple 'error "You are already ready!" (list player-id)))
            (progn
              (broadcast state 'ready player-id)
              (! http-id 'ok)))
          (run (coll:update-in state `(players ,player-id) (update-room-player-ready? player 'true))))
         ((tuple 'error _)
          (! http-id (tuple 'error (io:lib_format "Player ~p needs to be connected in order to be ready!" (list player-id))))
          (run state)))))
    (`#(terminate ,http-id ,player-id)
       (if (== player-id 1)
         (progn
           (broadcast state 'end)
           (! http-id 'ok)
           (run (initial)))
         (progn
           (! (room-player-pid (coll:get-in state `(players ,player-id))) (tuple 'error "You can't terminate the room: you don't own it!"))
           (! http-id (tuple 'error "Somebody tried to terminate the room whilst not owning it."))
           (run state))))
    (`#(start ,http-id ,player-id)
     (io:format "Somebody wants to start: ~p\n" (list player-id))
     (let* ((players (mref state 'players))
            (ready-players (maps:filter (lambda (_ player) (room-player-ready? player)) players))
            (ready-players-count (maps:size ready-players)))
       (if (and (== ready-players-count 4) (== player-id 1))
         (let* ((players-pids (lists:map (lambda (player-number) (clj:-> players (mref player-number) (room-player-pid))) '(1 2 3 4)))
                (initial-game-state (game:initial-game players-pids))
                (decider-id (spawn 'game 'decider (list initial-game-state))))
           (broadcast state 'all-ready! initial-game-state)
           (io:format "This is the initial game: ~p\n" (list initial-game-state))
           (! http-id 'ok)
           (run (mset state 'decider decider-id)))
         (let ((player (mref players player-id))
               (error-msg (if (not (== player-id 1))
                               "Someone started without being owner of the room!"
                               "Not all players are ready!")))
           (! http-id (tuple 'error error-msg))           
           (! (room-player-pid player) (tuple 'error error-msg))
           (run state)))))
    (`#(play ,action ,http-id ,player-id)
     (let ((player (coll:get-in state `(players ,player-id))))
       (case (coll:mref-safe state 'decider)
         ((tuple 'ok decider-id)
          (case (xml:read-action-params action player-id)
            ((tuple 'error unknown)
             (let ((error-msg (io_lib:format "Unrecognized input: ~p" (list unknown))))
               (! http-id (tuple 'error error-msg))
               (! (room-player-pid player) (tuple 'error error-msg))))
            (params
             (io:format "Somebody wants to play: ~p\n~p\n" (list player-id params))
             (! decider-id params)
             (! http-id 'ok))))
         ((tuple 'error _)
          (! (room-player-pid player) (tuple 'error "You can't play: decider is not ready!")))))
     (run state))))

