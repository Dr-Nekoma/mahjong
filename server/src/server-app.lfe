(defmodule server-app
  (behaviour application)
  ;; app implementation
  (export
   (start 2)
   (stop 1))
  (import
   (from game (play 1))
   (from session (blah 1))
   ))

;;; --------------------------
;;; application implementation
;;; --------------------------

(defun start (_type _args)
  "start the application."
  (let* ((game (spawn 'game 'play `(,(game:initial-game))))
	 ;; (name-server (spawn 'session 'name-server `(,(map))))
         (dispatch  (cowboy_router:compile `[#(_ [#("/" session '())])]))
         (`#(ok ,_) (cowboy:start_clear 'http '[#(port 4040)]
                      (map 'env (map 'dispatch dispatch)))))
    (server-sup:start_link)))

(defun stop (_state)
  (server-sup:stop)
  'ok)