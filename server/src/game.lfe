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
   (cond
    ((clj:map? mapp) (get-in (map-get mapp head) tail))
    ((erlang:is_list mapp) (get-in (lists:nth head mapp) tail))
    (else (get-in (tref mapp head) tail)))))

(defun update-nth
  (((cons head tail) '1 f) (cons (funcall f head) tail))
  (((cons head tail) n f) (when (< 1 n)) (cons head (update-nth tail (- n 1) f))))

(defun update-in
  ((_ '() value) value)
  ((mapp (cons key tail) value)
   (cond
    ((clj:map? mapp) (map-update mapp key (update-in (map-get mapp key) tail value)))
    ((and (erlang:is_list mapp)
	  (erlang:is_integer key))
     (update-nth mapp key (lambda (x) (update-in x tail value))))
    (else (tset mapp key (update-in (tref mapp key) tail value))))))

(defun remove
  (((cons head tail) '1) tail)
  (((cons head tail) n) (when (< 1 n))
   (cons head (remove tail (- n 1)))))

(defun initial-player() (map 'hand (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
			     'discard-pile (list)
			     'open-hand (list)))

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

(defun kan ())

(defun chi ())

(defun pon ())

(defmacro can-play? (state player error-msg callback)
  `(let ((current (map-get ,state 'current)))
     (if (== ,player current)
       ,callback
       (game-error pid ,error-msg))))

(defun check-opening-hand (state player pid)
  (can-play? state player "Cannot open your hand."
	     (let* ((current-hand (list 'players current 'hand))
		    (hand (get-in state current-hand))
		    (next-state state)
	       (game-loop pid next-state))))

(defun check-discard-action (state index player pid)
  (let ((current (map-get state 'current)))
    (if (== player current)
      (let* ((current-hand (list 'players current 'hand))
	     (current-pile (list 'players current 'discard-pile))
	     (hand (get-in state current-hand))
	     (next-state (clj:-> state
			      (update-in current-hand (remove hand index))
			      (update-in current-pile (cons (lists:nth index hand)
							    (get-in state current-pile))))))
	(game-loop pid next-state))
      (game-error pid "You can't discard right now"))))

;; TODO: When discard we must calculate available pon, kan, chi for all the other players
;; to inform FE that they can click to make these moves
;; TODO: Have two separate processes one for auth another for the game
(defun play (state)
    (receive
      ((tuple 'discard index player pid) (check-discard-action state index player pid))
      ((tuple 'open-hand player pid) (check-opening-hand state player pid))
      ((tuple 'open-hand player piece pid) state)))

