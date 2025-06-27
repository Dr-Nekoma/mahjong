(defmodule game
  ;; app implementation
  (export
   (play 1)
   (initial-game 0)))

;; { 'sessions => [ {'current => 0
;; 		 'east => 1
;; 		 'name => string
;; 		 'password => string
;; 		 'players => {{
;; 		 'points => 0
;; 		 'hand => []
;;    		 'discard => []
;; 		 },
;; 		 {
;; 		 'points => 0		 
;; 		 'hand => []
;;    		 'discard => []
;; 		 },
;; 		 {
;; 		 'points => 0		 
;; 		 'hand => []
;;    		 'discard => []
;; 		 },
;; 		 {
;; 		 'points => 0		 
;; 		 'hand => []
;;    		 'discard => []
;; 		 }}
;; 		 'wall => []
;; 		 }	       
;; 	       ]
;; }

(defun get-in
  ((mapp '()) mapp)
  ((mapp (cons head tail))
   (if (clj:map? mapp)
     (get-in (map-get mapp head) tail)
     (get-in (tref mapp head) tail))))

(defun update-in
  ((_ '() value) value)
  ((mapp (cons key tail) value)
   (if (clj:map? mapp)
     (map-update mapp key (update-in (map-get mapp key) tail value))
     (tset mapp key (update-in (tref mapp key) tail value)))))

(defun remove
  (((cons head tail) '1) tail)
  (((cons head tail) n) (when (< 1 n))
   (cons head (remove tail (- n 1)))))

(defun initial-player() (map 'hand (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14)))

(defun initial-game() (map 'current 1
			   'players (tuple
				     (initial-player)
				     (initial-player)
				     (initial-player)
				     (initial-player))))

(defmacro game-error (pid msg)
  `(progn
     (! ,pid #(error ,msg))
     (play state)))

(defun next-player (state)
  (map-update state 'current (clj:-> state (map-get 'current) (+ 1) (rem 4))))

(defmacro game-loop (pid next-state)
  `(progn
     (! ,pid (tuple 'success ,next-state))
     (play (next-player ,next-state))))

;; TODO: Have two separate processes one for auth another for the game
;; TODO: Add discarded piece into discarded pile of player
;; TODO: When discard we must calculate available pon, kan, chi to inform FE that they can click to make
;; these moves
(defun play (state)
    (receive
      ((tuple 'discard index player pid)
       (let ((current (map-get state 'current)))
	 (if (== player current)
	   (let* ((current-hand (list 'players current 'hand))
		  (hand (get-in state current-hand))
		  (next-state (update-in state current-hand (remove hand index))))
	     (game-loop pid next-state))
	   (game-error pid "You can't discard right now"))))
      ((tuple 'draw ))))

