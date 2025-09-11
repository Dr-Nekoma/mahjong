(defmodule server-app
  (behaviour application)
  ;; app implementation
  (export
   (start 2)
   (stop 1)))

;;; --------------------------
;;; application implementation
;;; --------------------------

(defun start (_type _args)
  "start the application."
  (let* ((dispatch  (cowboy_router:compile `[#(_ [#("/" session ,(map 'room (spawn 'session 'room (list (session:initial-room)))))])]))
         (`#(ok ,_) (cowboy:start_clear 'http '[#(port 4040)]
                      (map 'env (map 'dispatch dispatch)))))
    (server-sup:start_link)))

(defun stop (_state)
  (server-sup:stop)
  'ok)
