(defmodule prelude
  (export-macro nlet)
  (export
   (times 3)
   (foldl-maybe 3)))

;; TODO: Figure out why we can't reuse this macro in other modules
(defmacro nlet
  (`(,label ,bindings . ,body)
   (when (is_atom label))
   `(fletrec ((,label ,(lists:map (fun car 1) bindings) ,@body))
      (,label ,@(lists:map (fun cadr 1) bindings)))))

(defun times
  ((0 f input) input)
  ((n f input) (when (< 0 n)) (times (- n 1) f (funcall f input))))

(defun foldl-maybe
  ((_ acc '()) (tuple 'ok acc))
  ((f acc (cons head tail))
   (case (funcall f acc head)
     (`#(ok ,value)
      (foldl-maybe f value tail))
     (`#(error ,value) (tuple 'error value)))))

