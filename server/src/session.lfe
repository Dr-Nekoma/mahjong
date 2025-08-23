(defmodule session
  (doc "REST hello world handler.")
  (export
    (init 2)
    (player 2)
    (full-state->player-state 2)
    (available-actions 1))
  (export (to_html 2))
  (module-alias (collections coll)))

(defun init (req opts)
  "Switch to the REST protocol and start executing the state machine."
  `#(cowboy_rest ,req ,opts))

(defun public-information ()
  '(discard-pile open-hand))

(defun extract-if-self (player-number)
  (lambda (player index)
    (if (== index player-number)
      player
      (maps:with (public-information) player))))

(defun full-state->player-state (state player-number)
  (let ((players (coll:tmap
                   (extract-if-self player-number)
                   (mref state 'players))))
    (mset (maps:with '(current-player) state)
      'players players
      'you player-number)))

(defun available-actions (player-state)
  (lists:flatmap
    (lambda (f)
      (funcall f player-state))
    (list
      ;; TODO: add more checks
      ;; TODO: abstract common pattern
      (lambda (player-state)
        (if (yaku:call-riichi?
              (coll:get-in player-state
              (list 'players (mref player-state 'you) 'hand)))
          (list 'call-riichi)
          (list))))))

;; state: hand[2-man, 3-man, 1-pin, ...], last-command['discard], last-player[1], whoami[2]
;; returns: ['can-call-chi]
(defun player (state number)
  (receive
    (`#(new-state ,state)
     (let ((visible-state (full-state->player-state state number)))
       ;; TODO: push this to the frontend
       (available-actions visible-state))
     (player state number))
    (unknown
     (io:format "Unknown message: ~p\n" (list unknown))
     (player state number))))

(defun to_html (req state)
  "Return a text hello."
  `#(#"REST Hello World as text!" ,req ,state))
