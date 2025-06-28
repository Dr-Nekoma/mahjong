(defmodule actions
  (export
   (discard 4)
   (open-hand 3))
  (module-alias (collections coll)))

(defun discard (state index player pid)
  (game:can-play? state player "Cannot discard from your hand."
	     (let* ((current-hand (list 'players current-player 'hand))
		    (current-pile (list 'players current-player 'discard-pile))
		    (hand (coll:get-in state current-hand))
		    (next-state (clj:-> state
					(coll:update-in current-hand (coll:remove hand index))
					(coll:update-in current-pile (cons (lists:nth index hand)
								      (coll:get-in state current-pile))))))
	       (game:loop pid next-state))))

;; (defun kan ())

;; (defun chi ())

;; (defun pon ())

(defun open-hand (state player pid)
  (game:can-play state player "Cannot open your hand."
		  (list)))
