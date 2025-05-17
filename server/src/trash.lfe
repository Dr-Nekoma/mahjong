(defmodule trash
  (export (init 2)
	  (content_types_provided 2)
	  (trash_xml 2)
	  (trash_json 2)))

;; (flet ((abc ((x y) (io:format "First")) ((x) (io:format "Second")))) (funcall (function abc 2) 1 2)

(defun init (req opts)
  (tuple 'cowboy_rest req opts))

(defun content_types_provided (req state)
  (tuple (list #(#"application/xml" trash_xml)
	       #(#"application/json" trash_json)) req state))

(defun trash_xml (req state)
  (tuple #"<?xml version=\"1.0\" encoding=\"UTF-8\"><message>hello! :)</message>" req state))


(defun trash_json (req state)
  (tuple #"{\"message\": \"hello! :)\"}" req state))