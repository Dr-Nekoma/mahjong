(defmodule sse
  (export
    (init 2)
    (info 3)))

(defun default-options (idle-minutes)
  (tuple 'set_options (map 'idle_timeout (* 60000 idle-minutes)
                           'reset_idle_timeout_on_send 'true)))

(defun init (req0 state)
  (let ((req (cowboy_req:stream_reply 200
               (map #"content-type" #"text/event-stream")
               req0))
        ((map 'room room-pid) state))
    (cowboy_req:cast (default-options 10) req0)
    (! room-pid (tuple 'connect (self)))
    (tuple 'cowboy_loop req state)))

(defun info
  (((tuple 'ok game-state) req state)
   (cowboy_req:stream_events (xml:serialize-event 'play game-state) 'nofin req)
   (tuple 'ok req state))
  (((tuple 'info msg) req state)
   (cowboy_req:stream_events (xml:serialize-event 'info (map 'message msg)) 'nofin req)
   (tuple 'ok req state))
  (((tuple 'error msg) req state)
   (cowboy_req:stream_events (xml:serialize-event 'error (if (is_list msg)
                                                           (map 'message msg)
                                                           msg)) 'nofin req)
   (tuple 'ok req state))
  (('end req state)
   (cowboy_req:stream_trailers (xml:serialize-event 'end (map)) req)
   (tuple 'stop req state)))
