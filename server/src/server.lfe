(defmodule server
  (export (start 0)))

(defun start ()
  "Ensure all dependencies have been started for the application."
  (application:ensure_all_started 'server))