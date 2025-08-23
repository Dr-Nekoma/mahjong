(defmodule prelude
  (export-macro nlet))

(defmacro nlet
  (`(,label ,bindings . ,body)
   (when (is_atom label))
   `(fletrec ((,label ,(lists:map (fun car 1) bindings) ,@body))
      (,label ,@(lists:map (fun cadr 1) bindings)))))
