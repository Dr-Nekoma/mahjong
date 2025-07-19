(defmodule actions
  (export
   (discard 1)
   (open-hand 1)
   (draw 1))
  (module-alias (collections coll)))

(defmacro defaction
  (`[,action-name ,args ,error-msg . ,body]
   `(defun ,action-name (arg)
      (let ((,`(map ,@(lists:merge
                        (lists:map (lambda (sym) (list `',sym sym))
                                   args)))
             arg))
        (let ((current-player (coll:get-in arg '(state current-player))))
          (if (== current-player (coll:get-in arg '(player)))
            (progn ,@body)
            (game:error (coll:get-in arg '(pid)) ,error-msg)))))))

(defaction discard (state player tile pid)
  "Cannot discard from your hand."
  (let* ((current-hand (list 'players current-player 'hand))
         (current-pile (list 'players current-player 'discard-pile))
         (hand (coll:get-in state current-hand))
         (tile-count (coll:mset-count hand tile))
         (next-state (clj:-> state
                             (coll:update-in current-hand (coll:mset-remove hand tile))
                             (coll:update-in current-pile (cons tile (coll:get-in state current-pile))))))
    (if (< tile-count 1)
      (game:error pid "Cannot discard chosen tile.")
      (game:loop pid next-state))))

;; Definition 1. Closed Hand: when you have all your pieces in your hand (14)
  ;; Winning Conditions:
    ;; 4 sets of 3 pieces each, straight or three-of-a-kind, plus a pair
    ;; 7 pairs
;; Definition 2. Open Hand: when you have some pieces in your hand AND some pieces on the table
;; in order to form kan OR chi OR pon.


;; (defun kan ())

;; (defun chi ())

;; (defun pon ())

;; closed hand win conditions
;; every set must be a flush

;; when you're one piece away from reaching that condition, you can call
;; riichi before discarding
;; after calling riichi, if you draw the tile you are missing, you earn a point

;;

; open hand win conditions

(defaction draw (state player pid)
  "Cannot draw"
  (let* ((current-hand (list 'players current-player 'hand))
         ((cons next-tile wall) (coll:get-in state '(wall)))
         (hand (coll:get-in state current-hand))
         (next-state (clj:-> state
                             (coll:update-in current-hand (coll:mset-add hand next-tile))
                             (coll:update-in '(wall) wall))))
    (game:loop pid next-state)))

(defaction open-hand (state player pid)
  "Cannot open your hand."
  ;; TODO
  (game:loop pid state))
