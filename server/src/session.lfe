(defmodule session
  (doc "REST hello world handler.")
  (export (init 2))
  (export (to_html 2)))

(defun init (req opts)
  "Switch to the REST protocol and start executing the state machine."
  `#(cowboy_rest ,req ,opts))

(defun to_html (req state)
  "Return a text hello."
  `#(#"REST Hello World as text!" ,req ,state))
