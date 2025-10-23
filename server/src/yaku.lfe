(defmodule yaku
  (export (riichi? 1)
          (call-riichi? 1)
          (full-flush? 1)
          (seven-pairs? 1)
          (get-sequence 2))
  (module-alias (collections coll)))

(include-lib "tile.lfe")

(defun triplet? (hand tile) (=< 3 (coll:mset-count hand tile)))

(defun get-sequence (hand tile)
  (if (tiles:number? tile)
       (let* ((start (tile-spec tile))
              (tile+1 (update-tile-spec tile (+ start 1)))
              (tile+2 (update-tile-spec tile (+ start 2)))
              (removal-count (clj:->> (list tile tile+1 tile+2)
                                      (lists:map (clj:partial (fun coll:mset-count 2) hand))
                                      (lists:min))))
         (map
          tile removal-count
          tile+1 removal-count
          tile+2 removal-count))
       (map)))

(defun eliminate-tile (partial-hand tile)
  (let ((check-done
         (lambda (partial-hand)
           (if (== 0 (coll:mset-count partial-hand tile))
             (tuple 'ok partial-hand)
             (tuple 'error partial-hand)))))
    (cond
      ((triplet? partial-hand tile)
       (eliminate-tile (coll:mset-minus partial-hand (map tile 3)) tile))
      ((tiles:number? tile)
       (funcall check-done (coll:mset-minus partial-hand (get-sequence partial-hand tile))))
      ('true (funcall check-done partial-hand)))))

(defun remove-pair (hand)
  (clj:->> hand
           (maps:filter (lambda (_ count) (=< 2 count)))
           (maps:keys)
           (lists:map (lambda (key)
                        (coll:mset-minus hand (map key 2))))))

(defun remaining-pair? (remaining)
  (and (== 2 (maps:size remaining))
       (== (maps:values remaining) (list 1 1))))

(defun remaining-triplet? (remaining)
  (and (== 2 (maps:size remaining))
       (or (== (maps:values remaining) (list 2 1))
           (== (maps:values remaining) (list 1 2)))))

(defun remaining-sequence? (remaining)
  (and (or (== 2 (maps:size remaining)) (== 3 (maps:size remaining)))
       (let ((sorted-remaining (clj:->> remaining
                                        (maps:keys)
                                        (lists:sort)
                                        (lists:filter (fun tiles:number? 1)))))
         (lists:foldl (lambda (tile sequence?)
                        (or sequence? (maps:is_key (update-tile-spec tile (+ 1 (tile-spec tile))) remaining)))
                      'false
                      sorted-remaining))))

(defun call-riichi? (hand)
  (let ((sorted-tiles (lists:sort (maps:keys hand))))
    (prelude:nlet recur ((partial-hands (cons hand (remove-pair hand))))
      (if (== partial-hands '())
        'false
        (let (((cons partial-hand partial-hands) partial-hands))
          (case (prelude:foldl-maybe (fun eliminate-tile 2) partial-hand sorted-tiles)
            ((tuple 'ok (map)) 'true)
            ((tuple 'error remaining) (or (remaining-pair? remaining)
                                          (remaining-triplet? remaining)
                                          (remaining-sequence? remaining)
                                          (recur partial-hands)))))))))

(defun full-flush? (hand) (clj:->> hand (maps:keys) (lists:map (fun tile-suit 1)) (sets:from_list) (sets:size) (== 1)))

(defun riichi? (hand)
  (let ((sorted-tiles (lists:sort (maps:keys hand))))
    (prelude:nlet recur ((partial-hands (remove-pair hand)))
      (if (== partial-hands '())
        'false
        (let (((cons partial-hand partial-hands) partial-hands))
          (case (prelude:foldl-maybe (fun eliminate-tile 2) partial-hand sorted-tiles)
            ((tuple 'ok (map)) 'true)
            (_ (recur partial-hands))))))))

(defun count-pairs (hand)
  (maps:fold (lambda (tile count acc) (+ acc (div count 2))) 0 hand))

(defun seven-pairs? (hand)
  (== 7 (count-pairs hand)))
