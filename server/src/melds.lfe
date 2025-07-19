(defmodule melds
  (export
   (three-of-a-kind? 2)
   (all-pairs? 1)
   (riichi-melds? 1)
   (count-straights 1)
   (count-three-of-a-kind 1))
  (module-alias (collections coll)))

(defrecord tile suit spec) ; TODO: deduplicate

(defmacro nlet
  (`(,label ,bindings . ,body)
   (when (is_atom label))
   `(fletrec ((,label ,(lists:map (fun car 1) bindings) ,@body))
      (,label ,@(lists:map (fun cadr 1) bindings)))))

(defun eliminate-tile (partial-hand tile)
  (let ((check-done
         (lambda (partial-hand)
           (if (== 0 (coll:mset-count partial-hand tile))
             (tuple 'ok partial-hand)
             'error))))
    (cond
      ((=< 3 (coll:mset-count partial-hand tile))
       (eliminate-tile
         (coll:mset-minus partial-hand (map tile 3))
         tile))
      ((lists:member
         (tile-suit tile)
         (tiles:numbered-suits))
       (let* ((start (tile-spec tile))
              (tile+1 (update-tile-spec tile (+ start 1)))
              (tile+2 (update-tile-spec tile (+ start 2)))
              (removal-count (clj:->> (list tile tile+1 tile+2)
                               (lists:map (lambda (t) (coll:mset-count partial-hand t)))
                               (lists:min))))
         (funcall check-done
           (coll:mset-minus partial-hand
             (map
               tile removal-count
               tile+1 removal-count
               tile+2 removal-count)))))
      ('true (funcall check-done partial-hand)))))

(defun foldl-maybe
  ([_ acc '()] (tuple 'ok acc))
  ([f acc (cons head tail)]
   (case (funcall f acc head)
     (`#(ok ,value)
      (foldl-maybe f value tail))
     ('error 'error))))

(defun riichi-melds? (hand)
  (let ((sorted-tiles (lists:sort (maps:keys hand))))
    (nlet recur ((partial-hands
                  (clj:->> hand
                    (maps:filter (lambda (_ count) (=< 2 count)))
                    (maps:keys)
                    (lists:map (lambda (key)
                                 (coll:mset-minus hand (map key 2)))))))
      (if (== partial-hands '())
        'false
        (let (((cons partial-hand partial-hands) partial-hands))
          (case (foldl-maybe (fun eliminate-tile 2) partial-hand sorted-tiles)
            ((tuple 'ok (map)) 'true)
            (_ (recur partial-hands))))))))

(defun count-straights (hand)
  (let ((pairs (clj:->> hand (maps:filter (lambda (tile count) (>= count 2)) hand) (maps:keys))))
    (maps:fold
     ()
     ()
     (lists:fold_left (lambda (pair acc) (maps:puts pair 2 acc)) (map) pairs))))

(defun count-three-of-a-kind (hand)
  (maps:fold (lambda (tile count acc) (if (== 3 count) (clj:inc acc))) 0 hand))

(defun three-of-a-kind? (hand three-of-a-kind)
  (== three-of-a-kind (count-three-of-a-kind hand)))

(defun count-pairs (hand)
  (maps:fold (lambda (tile count acc) (+ acc (div count 2))) 0 hand))

(defun all-pairs? (hand)
  (== 7 (count-pairs hand)))
