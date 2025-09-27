(defmodule connect
  (export
    (init 2)
    (info 3)))

(defun init (req0 state)
  (let ((req (cowboy_req:stream_reply 200
               (map #"content-type" #"text/event-stream" #"access-control-allow-origin" #"*")
               req0))
        ((map 'room room-pid) state))
    (cowboy_req:cast (tuple 'set_options (map 'idle_timeout 6000000
					      'reset_idle_timeout_on_send 'true)) req0)
    (! room-pid (tuple 'connect (self)))
    (tuple 'cowboy_loop req state)))

(defun serialize-tuple-xml
  (((tuple 'player-number player-number)) (tuple 'player-number (erlang:integer_to_list player-number)))
  ((catchall) catchall))

(defun serialize-xml (event mapp)
  (clj:-> event
	  (tuple
	   (clj:->> mapp
		    (maps:to_list)
		    (lists:map (fun serialize-tuple-xml 1))) '())
	  (list)
	  (xmerl:export_simple 'xmerl_xml)))

(defun serialize
  (('play (tuple player-number gamestate))
   (map 'data (session:serialize-game player-number gamestate)))
  ((event data)
   (clj:->> data (serialize-xml event) (map 'data))))

(defun info
  (((tuple 'connected player-number) req state)
   (cowboy_req:stream_events (serialize 'connected
					(map 'player-number player-number)) 'nofin req)
   (tuple 'ok req state))
  (((tuple 'ok game-state) req state)
   (cowboy_req:stream_events (serialize 'play game-state) 'nofin req)
   (tuple 'ok req state))
  (((tuple 'info msg) req state)
   (cowboy_req:stream_events (serialize 'info (map 'message msg)) 'nofin req)
   (tuple 'ok req state))
  (((tuple 'error msg) req state)
   (cowboy_req:stream_events (serialize 'error (map 'message msg)) 'nofin req)
   (tuple 'ok req state))
  (('end req state)
   (cowboy_req:stream_trailers (serialize 'end (map)) req)
   (tuple 'stop req state)))
