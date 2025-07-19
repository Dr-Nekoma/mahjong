(defmodule tiles
  (export all))

(include-lib "records.lfe")

(defun numbered-suits ()
  (list 'bamboo 'circle 'character))

(defun wind-directions ()
  (list 'north 'south 'east 'west))

(defun dragon-colors ()
  (list 'red 'green 'white))

(defun shuffle (tiles)
  (clj:->> tiles
    (lists:map (lambda (tile) (tuple (rand:uniform) tile)))
    (lists:sort)
    (lists:map (lambda (pair) (tref pair 2)))))

(defun initial-wall ()
  (lists:merge
    (lc ((<- _ (lists:seq 1 4)))
      (lists:merge
        (list
          (lc ((<- color (dragon-colors)))
            (record tile suit 'dragon spec color))
          (lc ((<- direction (wind-directions)))
            (record tile suit 'wind spec direction))
          (lc ((<- suit (numbered-suits))
               (<- number (lists:seq 1 9)))
            (record tile suit suit spec number)))))))
