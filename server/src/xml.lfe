(defmodule xml
  (export (one-element 1)))

(defun one-element (serialized-element)
  (try
    (let ((node (clj:-> serialized-element
                (unicode:characters_to_list 'utf8)
                (xmerl_scan:string)
                (tref 1))))
      (case (xmerl_xs:select "name()" node)
        (`#(xmlObj string ,tag)
         (tuple (erlang:list_to_atom tag)
                (lists:foldl
                 (lambda (attribute acc)
                   (let ((name (tref attribute 2))
                         (value (tref attribute 9)))
                     (mset acc name (one-element value))))
                 (map)
                 (xmerl_xs:select "@*" node))))))
    (catch (`#(,type ,value ,stacktrace)
            serialized-element))))
