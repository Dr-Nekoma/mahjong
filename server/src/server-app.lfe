(defmodule server-app
  (behaviour application)
  (export
   (start 2)
   (stop 1)))

(defun start (_type _args)
  "start the application."
  (let* ((session-state (map 'room (spawn 'room 'run (list (room:initial)))))
         (dispatch  (cowboy_router:compile `[#(_ [#("/" session ,session-state)
                                                  #("/connect" sse ,session-state)])]))
         (`#(ok ,_) (cowboy:start_clear 'http '[#(port 4040)]
                      (map 'env (map 'dispatch dispatch)))))
    (server-sup:start_link)))

(defun stop (_state)
  (server-sup:stop)
  'ok)
