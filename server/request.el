(require 'request)

(defvar *response* nil)
(defvar *succ* nil)

(setf *response*
      (request "localhost:4040/"
	:type "POST"
	:data '(("key1" . "value1"))
	:parser 'buffer-string
	:success (cl-function (lambda (&key data &allow-other-keys)
                 (when data
                   (with-current-buffer (get-buffer-create "*request demo*")
                     (erase-buffer)
                     (insert data)
                     (pop-to-buffer (current-buffer))))))
	:error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
			      (message "Got error: %S" error-thrown)))
	:complete (lambda (&rest _) (message "Finished!"))))

(message "%S" *response*)
