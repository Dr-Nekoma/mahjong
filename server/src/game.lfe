(defmodule game
  (export
   (play 1)
   (initial-game 0)
   (next-player 1))
  (export-macro can-play? loop error))

(defun initial-player (hand)
  (map
    'hand hand
    'discard-pile (list)
    'open-hand (list)))

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
     (game:play state)))

(defun next-player (state)
  (map-update state 'current-player (clj:-> state (map-get 'current-player) (+ 1) (rem 4))))

(defmacro loop (pid next-state)
  `(progn
     (! ,pid (tuple 'success ,next-state))
     (game:play (game:next-player ,next-state))))

(defmacro can-play? (state player error-msg form)
  `(let ((current-player (map-get ,state 'current-player)))
     (if (== ,player current-player)
       ,form
       (game:error pid ,error-msg))))

;; TODO: When discard we must calculate available pon, kan, chi for all the other players
;; to inform FE that they can click to make these moves
;; TODO: Have two separate processes one for auth another for the game
(defun play (state)
    (receive
      ((tuple 'discard index player pid) (actions:discard state player index pid))
      ((tuple 'draw player pid) (actions:draw state player pid))
      ((tuple 'open-hand player pid) (actions:open-hand state player pid))
      ((tuple 'open-hand player piece pid) state)))
