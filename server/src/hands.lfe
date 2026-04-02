(defmodule hands
  (export (seven-pairs 0))
  (module-alias (collections coll)))

(defun seven-pairs ()
  (lists:foldl
    (lambda (tile acc)
      (coll:mset-plus acc (map tile 2)))
    (map)
    (lists:map (lambda (n) (tuple 'tile 'bamboo n))
               '(1 2 3 4 5 6 7))))
