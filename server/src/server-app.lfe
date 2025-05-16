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
  (let* ((dispatch  (cowboy_router:compile '[#(_ [#("/" handler [])])]))
         (`#(ok ,_) (cowboy:start_http 'http 100 '[#(port 4040)]
                      `[#(env [#(dispatch ,dispatch)])])))
    (server-sup:start_link)))

(defun stop (_state)
  (server-sup:stop)
  'ok)
