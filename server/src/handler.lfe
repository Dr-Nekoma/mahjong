(defmodule handler
  (doc "REST hello world handler.")
  ;; Cowboy handler
  (export (init 2))
  ;; REST callbacks
  (export (content_types_provided 2))
  ;; API
  (export (hello->html 2)
          (hello->json 2)
          (hello->text 2)))

;;;===================================================================
;;; Cowboy handler
;;;===================================================================

(defun init (req opts)
  "Switch to the REST protocol and start executing the state machine."
  `#(cowboy_rest ,req ,opts))


;;;===================================================================
;;; REST callbacks
;;;===================================================================

(defun content_types_provided (req state)
  (io:format "~p\n" (list state))
  `#([#(#"text/html"        hello->html)
      #(#"application/json" hello->json)
      #(#"text/plain"       hello->text)]
     ,req ,state))


;;;===================================================================
;;; API
;;;===================================================================

(defun hello->html (req state)
  "Return hello in HTML."
  (! state (tuple 'draw (map 'player 1 'pid (self))))
  (receive
    ((tuple 'success new-state)
     (io:format "Success: ~p\nWall length: ~p\n"
                (list new-state (length (map-get new-state 'wall)))))
    ((tuple 'error msg) (io:format "Error: ~p\n" (list msg)))
    (anything (io:format "Catchall: ~p\n" (list anything))))  
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

(defun hello->json (req state)
  "Return a JSON-formatted hello."
  (! state "JSON")
  (let ((body #"{\"rest\": \"Hello World!\"}"))
    `#(,body ,req ,state)))

(defun hello->text (req state)
  "Return a text hello."
  (! state (tuple 'discard 2 1 (self)))
  (receive
    ((tuple 'success new-state) (io:format "Success: ~p\n" (list new-state)))
    ((tuple 'error msg) (io:format "Error: ~p\n" (list msg)))
    (anything (io:format "Catchall: ~p\n" (list anything))))
  `#(#"REST Hello World as text!" ,req ,state))
