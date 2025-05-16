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
  (logger:set_application_level 'server 'all)
  (logger:info "Starting server application ...")
  (server-sup:start_link))

(defun stop (_state)
  (server-sup:stop)
  'ok)
