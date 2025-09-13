(defmodule connect
  (export
    (init 2)
    (info 3)))

(defun init (req state)
  (let ((req (cowboy_req:stream_reply 200
               (map #"content-type" #"text/event-stream")
               req))
        ((map 'room room-pid) state))
    (! room-pid (tuple 'connect (self)))
    (tuple 'cowboy_loop req state)))

(defun info
  (((tuple 'connected player-number) req state)
   (cowboy_req:stream_events (map 'player-number player-number) 'nofin req)
   (tuple 'ok req state))
  (((tuple 'ok game-state) req state)
   (cowboy_req:stream_events game-state 'nofin req)
   (tuple 'ok req state))
  (((tuple 'info msg) req state)
   (cowboy_req:stream_events (map 'info msg) 'nofin req)
   (tuple 'ok req state))
  (((tuple 'error msg) req state)
   (cowboy_req:stream_events (map 'error msg) 'nofin req)
   (tuple 'ok req state))
  (('end req state)
   (cowboy_req:stream_trailers (map) req)
   (tuple 'stop req state)))
