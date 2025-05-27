(defmodule server-app
  (behaviour application)
  ;; app implementation
  (export
   (start 2)
   (play 1)
   (stop 1)))

;;; --------------------------
;;; application implementation
;;; --------------------------

(defun play (state)
  (receive
    (_ (progn
	 (io:format "I've passed in here buddy xD!")
	 (play state)))))

(defun start (_type _args)
  (let* ((game (spawn 'server-app 'play `(,(map 'session (self)))))
	 (dispatch  (cowboy_router:compile `#(_ [#("/" handler [,game])]))))
         (`#(ok ,_) (cowboy:start_clear 'http (list #(port 4040))
                      (map 'env (map 'dispatch dispatch)))))
    (server-sup:start_link)))

(defun stop (_state)
  (server-sup:stop)
  'ok)
