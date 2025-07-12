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

(defaction discard (state player index pid)
  "Cannot discard from your hand."
  (let* ((current-hand (list 'players current-player 'hand))
         (current-pile (list 'players current-player 'discard-pile))
         (hand (coll:get-in state current-hand))
         (next-state (clj:-> state
                             (coll:update-in current-hand (coll:remove hand index))
                             (coll:update-in current-pile (cons (lists:nth index hand)
                                                            (coll:get-in state current-pile))))))
    (game:loop pid next-state)))

;; (defun kan ())

;; (defun chi ())

;; (defun pon ())

(defaction draw (state player pid)
  "Cannot draw"
  (let* ((current-hand (list 'players current-player 'hand))
         ((cons next-tile wall) (coll:get-in state '(wall)))
         (hand (coll:get-in state current-hand))
         (next-state (clj:-> state
                             (coll:update-in current-hand (cons next-tile hand))
                             (coll:update-in '(wall) wall))))
    (game:loop pid next-state)))

(defaction open-hand (state player pid)
  "Cannot open your hand."
  ;; TODO
  (game:loop pid state))
