(defmodule game
  (export
   (decider 1)
   (initial-game 1)
   (loop 1)
   (error 3)
   (public-information 0)
   (full-state->player-state 2)
   (next-player 1))
  (module-alias (collections coll)))

(defun public-information ()
  '(discard-pile open-hand))

(defun extract-if-self (player-number)
  (lambda (player index)
    (if (== index player-number)
      (mset player 'available-actions (available-actions player player-number))
      (maps:with (game:public-information) player))))

(defun full-state->player-state (state player-number)
  (let ((players (coll:tmap
                   (extract-if-self player-number)
                   (mref state 'players))))
    (mset (maps:with '(current-player) state)
      'players players
      'you player-number)))

;; TODO: When discard we must calculate available pon, kan, chi for all the other players
;; to inform FE that they can click to make these moves.
(defun available-actions (player player-number)
  (lists:flatmap
    (lambda (f)
      (funcall f player))
    (list
      ;; TODO: add more checks
      ;; TODO: abstract common pattern
      (lambda (player)
        (if (yaku:call-riichi? (mref player 'hand))
          (list 'call-riichi)
          (list))))))

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

(defun split-hand
  (((tuple hands wall))
   (let (((tuple hand remaining-wall) (lists:split 13 wall)))
     (tuple (cons hand hands) remaining-wall))))

(defun initial-game (players-pids)
  (let* (((tuple hands wall) (clj:->> (tiles:initial-wall)
                                      (tiles:shuffle )
                                      (tuple (list))
                                      (prelude:times 4 (function split-hand 1))))
          (players (clj:->> players-pids (lists:zipwith (function initial-player 2) hands) (list_to_tuple))))
    (map
      'current-player 1
      'wall wall
      'players players)))

(defun decider (state)
  (receive
    ((tuple 'discard params) (actions:discard (map-set params 'state state)))
    ((tuple 'draw params) (actions:draw (map-set params 'state state)))
    ((tuple 'riichi params) (actions:riichi (map-set params 'state state)))))

(defun error (state player msg)
  (! (coll:get-in state `(players ,player pid)) (tuple 'error msg))
  (decider state))

(defun next-player (state)
  (map-update state 'current-player (clj:-> state (mref 'current-player) (+ 1) (rem 4))))

(defun broadcast (state)
  (coll:tmap (lambda (player _)
               (let ((player-process (mref player 'pid)))
                 (! player-process (tuple 'new-state state))))
             (mref state 'players)))

(defun loop (next-state)
  (broadcast next-state)
  (clj:->> next-state
           (next-player)
           (decider)))
