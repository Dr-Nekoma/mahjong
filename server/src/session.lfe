(defmodule session
  (doc "REST hello world handler.")
  (export
    (init 2)
    (player 3)
    (waiting-player 2))
  (export (allowed_methods 2)
          (handler 2)
          (content_types_accepted 2))
  (module-alias (collections coll)))

(defun init (req opts)
  "Switch to the REST protocol and start executing the state machine."
  `#(cowboy_rest ,req ,opts))

(defun allowed_methods (req state)
  `#([#"GET" #"HEAD" #"OPTIONS" #"POST"] ,req ,state))

(defun content_types_accepted (req state)
  `#([#(#"application/xml" handler)] ,req ,state))

(defun player (state number sse-pid)
  (receive
    (`#(new-state ,state)
     (let ((visible-state (game:full-state->player-state state number)))
       (! sse-pid (tuple 'ok (tuple number visible-state)))
       (player state number sse-pid)))
    (`#(info ,msg)
     (! sse-pid (tuple 'info msg))
     (player state number sse-pid))
    ('end (! sse-pid 'end))
    (unknown
     (io:format "Unknown message: ~p\n" (list unknown))
     (player state number sse-pid))))

(defun waiting-player (number sse-pid)
  (receive
    (`#(ready ,another-player-number)
     (! sse-pid
       (tuple 'info
         (io_lib:format "Player ~p is ready!" (list another-player-number))))
     (waiting-player number sse-pid))
    ('end (! sse-pid 'end))
    (`#(info ,msg)
     (! sse-pid (tuple 'info msg))
     (waiting-player number sse-pid))
    (`#(all-ready! ,initial-state)
     (io:format "Everything set! Ready, steady, go!\n" (list))
     (! (self) `#(new-state ,initial-state))
     (player initial-state number sse-pid))))

(defun handle-room-response (req state)
  (receive
    ('ok `#(true ,req ,state))
    (`#(error ,msg)
     (io:format "Errored out: ~p\n" (list msg))
     `#(false ,req ,state))))

(defun handler (req state)
  (let* ((room-pid (mref state 'room))
         (body (clj:-> (cowboy_req:read_body req)
                       (tref 2)
                       (xml:read-one-element))))
    (case body
      (`#(terminate #m(player-id ,id) ())
       (! room-pid (tuple 'terminate (self) (list_to_integer id)))
       (handle-room-response req state))
      (`#(ready #m(player-id ,id) ()) ; TODO: read ID from the cookie
       (! room-pid (tuple 'ready (self) (list_to_integer id)))
       (handle-room-response req state))
      (`#(start #m(player-id ,id) ()) ; TODO: read ID from the cookie
       (! room-pid (tuple 'start (self) (list_to_integer id)))
       (handle-room-response req state))
      (`#(play #m(player-id ,id) (,action))
       (! room-pid (tuple 'play action (self) (list_to_integer id)))
       (receive ('ok `#(true ,req ,state)))))))
