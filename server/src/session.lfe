(defmodule session
  (doc "REST hello world handler.")
  ;; Cowboy handler
  (export (init 2))
  ;; REST callbacks
  (export (content_types_provided 2))
  ;; API
  (export (hello->html 2)))

(defun init (req opts)
  "Switch to the REST protocol and start executing the state machine."
  `#(cowboy_rest ,req ,opts))

(defun content_types_provided (req state)
  (io:format "~p\n" (list state))
  `#([#(#"text/html"        hello->html)
      #(#"application/json" hello->json)
      #(#"text/plain"       hello->text)]
     ,req ,state))

(defun hello->html (req state)
  "Return hello in HTML."
  (let ((body #"<html>
<head>
  <meta charset=\"utf-8\">
  <title>REST Hello world!</title>
</head>
<body>
  <p>REST Hello world as HTML!</p>
</body>
</html>"))
    `#(,body ,req ,state)))