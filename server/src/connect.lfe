(defmodule connect
  (export
    (init 2)
    (info 3)))

(defun init (req0 state)
  (let ((req (cowboy_req:stream_reply 200
               (map #"content-type" #"text/event-stream")
               req0))
        ((map 'room room-pid) state))
    (cowboy_req:cast (tuple 'set_options (map 'idle_timeout 600000)) req0)
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
