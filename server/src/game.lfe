(defmodule game
  (export
   (decider 1)
   (initial-game 0)
   (next-player 1))
  (export-macro loop error)
  (module-alias (collections coll)))

(defun initial-player (hand-list)
  (map
    'hand (lists:foldl (lambda (tile hand)
                         (coll:mset-add hand tile))
                       (coll:mset-empty)
                       hand-list)
    'discard-pile (list)
    'open-hand (list)
    'yaku-han (map)
    'stick-deposit 0))

(defun times
  ((0 f input) input)
  ((n f input) (when (< 0 n)) (times (- n 1) f (funcall f input))))

(defun split-hand
  (((tuple hands wall))
   (let (((tuple hand remaining-wall) (lists:split 14 wall)))
     (tuple (cons hand hands) remaining-wall))))

(defun initial-game ()
  (let* (((tuple hands wall) (clj:->> (tiles:shuffle (tiles:initial-wall))
                               (tuple (list))
                               (times 4 (function split-hand 1)))))
    (map
      'current-player 1
      'wall wall
      'players (list_to_tuple (lists:map (function initial-player 1) hands)))))

(defmacro error (pid msg)
  `(progn
     (! ,pid #(error ,msg))
     (game:decider state)))

(defun next-player (state)
  (map-update state 'current-player (clj:-> state (map-get 'current-player) (+ 1) (rem 4))))

(defmacro loop (pid next-state)
  `(progn
     (! ,pid (tuple 'success ,next-state))
     ;; TODO: send the state to the player processes
     (game:decider (game:next-player ,next-state))))

;; TODO: When discard we must calculate available pon, kan, chi for all the other players
;; to inform FE that they can click to make these moves
;; TODO: Have two separate processes one for auth another for the game
(defun decider (state)
  (receive
    ((tuple 'discard params) (actions:discard (map-set params 'state state)))
    ((tuple 'draw params) (actions:draw (map-set params 'state state)))
    ((tuple 'riichi params) (actions:riichi (map-set params 'state state)))
    ;; ((tuple 'win params) (actions:win ()))
    ((tuple 'open-hand params) (actions:open-hand (map-set params 'state state)))))
