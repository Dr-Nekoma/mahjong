(defmodule melds
  (export
   (three-of-a-kind? 2)
   (all-pairs 1))
  (module-alias (collections coll)))

(defun dealer (pid)
  (receive
    ((tuple '() _) (! pid 'false))
    ((tuple (cons tile next-tiles) melds) ())
    ))

3 3 3 3
4
5 5 5 5
6

(defun check-melds (tiles melds)
  (cond
   ((and (= melds 4) (null tiles)) t)
   ((or (> melds 4) (< (length tiles) 3)) nil)
   (t (let ((tile (car tiles)))
    (or
     ;; for 3 equal pieces
     (when (>= (count-tile tile tiles) 3)
           (check-melds (remove-n tile 3 tiles) (1+ melds)))
     ;; for a sequence of 3
     (let ((sequence-of-three (make-sequence-of-three tile)))
           (when (and sequence-of-three
              (cl-every (lambda (x) (member x tiles)) sequence-of-three))
             (let ((reduced tiles))
               (dolist (x sequence-of-three)
         (setq reduced (remove-n x 1 reduced)))
               (check-melds reduced (1+ melds))))))))))

(defun count-straights (hand)
  (let ((pairs (clj:->> hand (maps:filter (lambda (tile count) (>= count 2)) hand) (maps:keys))))
    (maps:fold
     ()
     ()
     (lists:fold_left (lambda (pair acc) (maps:puts pair 2 acc)) (map) pairs))))


(defun count-three-of-a-kind (hand)
  (maps:fold (lambda (tile count acc) (if (== 3 count) (clj:inc acc)) 0 hand)))

(defun three-of-a-kind? (hand three-of-a-kind)
  (== three-of-a-kind (count-three-of-a-kind hand)))

(defun count-pairs (hand)
  (maps:fold (lambda (tile count acc) (+ acc (div count 2))) 0 hand))

(defun all-pairs? (hand)
  (== 7 (count-pairs hand)))
