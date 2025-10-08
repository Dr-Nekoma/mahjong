(defmodule game
  (export
   (decider 1)
   (initial-game 1)
   (loop 1)
   (error 3)
   (next-player 1))
  (module-alias (collections coll)))

(defun initial-player (hand-list pid)
  (map
    'hand (lists:foldl (lambda (tile hand)
                         (coll:mset-add hand tile))
                       (coll:mset-empty)
                       hand-list)
    'pid pid
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

(defun initial-game (players-pids)
  (let* (((tuple hands wall) (clj:->> (tiles:shuffle (tiles:initial-wall))
                               (tuple (list))
                               (times 4 (function split-hand 1)))))
    (map
      'current-player 1
      'wall wall
      'players (list_to_tuple (lists:zipwith (function initial-player 2) hands players-pids)))))

;; TODO: When discard we must calculate available pon, kan, chi for all the other players
;; to inform FE that they can click to make these moves
;; TODO: Have two separate processes one for auth another for the game
(defun decider (state)
  (receive
    ((tuple 'discard params) (actions:discard (map-set params 'state state)))
    ((tuple 'draw params) (actions:draw (map-set params 'state state)))
    ((tuple 'riichi params) (actions:riichi (map-set params 'state state)))))

(defun error (state player msg)
  (! (coll:get-in state `(players ,player pid)) (tuple 'error msg))
  (decider state))

(defun next-player (state)
  (map-update state 'current-player (clj:-> state (map-get 'current-player) (+ 1) (rem 4))))

(defun loop (next-state)
  (coll:tmap (lambda (player _)
               (let ((player-process (map-get player 'pid)))
                 (! player-process (tuple 'new-state next-state))))
             (map-get next-state 'players))
  (clj:->> next-state
           (next-player)
           (decider)))
