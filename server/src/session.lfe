(defmodule session
  (doc "REST hello world handler.")
  (export (init 2))
  (export (to_html 2)))

(defun init (req opts)
  "Switch to the REST protocol and start executing the state machine."
  `#(cowboy_rest ,req ,opts))

;; state: hand[2-man, 3-man, 1-pin, ...], last-command['discard], last-player[1], whoami[2]
;; returns: ['can-call-chi]
(defun player (state)
  (list 'can-call-chi))

(defun to_html (req state)
  "Return a text hello."
  `#(#"REST Hello World as text!" ,req ,state))
