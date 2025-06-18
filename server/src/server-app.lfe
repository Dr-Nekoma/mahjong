(defmodule server-app
  (behaviour application)
  ;; app implementation
  (export
   (start 2)
   (stop 1))
  (import
    (from game (play 1)))
  )

;;; --------------------------
;;; application implementation
;;; --------------------------

(defun start (_type _args)
  "Start the application."
  (let* ((game (spawn 'game 'play `(,(map 'session (self)))))
         (dispatch  (cowboy_router:compile `[#(_ [#(_ handler ,game)])]))
         (`#(ok ,_) (cowboy:start_clear 'http '[#(port 4040)]
                      (map 'env (map 'dispatch dispatch)))))
    (server-sup:start_link)))

(defun stop (_state)
  (server-sup:stop)
  'ok)
