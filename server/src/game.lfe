(defmodule game
  (export
   (play 1)
   (initial-game 0)
   (next-player 1))
  (export-macro can-play? loop error))

(defun initial-player() (map 'hand (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
			     'discard-pile (list)
			     'open-hand (list)))

(defun initial-game() (map 'current-player 1
			   'players (tuple
				     (initial-player)
				     (initial-player)
				     (initial-player)
				     (initial-player))))

(defmacro error (pid msg)
  `(progn
     (! ,pid #(error ,msg))
     (play state)))

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
      ((tuple 'discard index player pid) (actions:discard state index player pid))
      ((tuple 'open-hand player pid) (actions:open-hand state player pid))
      ((tuple 'open-hand player piece pid) state)))

