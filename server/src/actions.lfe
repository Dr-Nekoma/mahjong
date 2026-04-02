(defmodule actions
  (export
   (discard 1)
   (draw 1)
   (riichi 1)
   (chii-options 2)
   (chii 1)
   (pon 1))
  (module-alias (collections coll)))

(include-lib "tile.lfe")

(defun async-action? (action-name)
  (or (== 'kan action-name)
      (== 'pon action-name)
      (== 'ron action-name)))

(defmacro defaction
  (`[,action-name ,args ,error-msg . ,body]
   `(defun ,action-name (arg)
      (let ((,`(map ,@(lists:merge
                        (lists:map (lambda (sym) (list `',sym sym))
                                   args)))
             arg))
        (let ((current-player (coll:get-in arg '(state current-player)))
              (player (coll:get-in arg '(player))))
          (if (or (== current-player player)
                  (async-action? ',action-name))
            ;; We are purposely making it opaque because the BEAM compiler is just too smart xD
            (case (clj:identity (progn ,@body))
              ((tuple 'ok next-state) (game:loop next-state))
              ((tuple 'error payload) (game:error state player payload)))
            (game:error state player ,error-msg)))))))

(defaction riichi (state player)
  "Cannot call riichi"
  (let* ((current-player-state (coll:get-in state (list 'players current-player)))
         ((map 'hand hand
               'yaku-han yaku-han
               'stick-deposit stick-deposit
               'open-hand open-hand)
          current-player-state)
         ;; TODO: Modify `yaku:call-riichi?` to check if hand has 14 tiles
         ;; If that is enforced, we don't need to check for the emptiness of the open hand
         (can-call-riichi? (and (yaku:call-riichi? hand)
                                (== 0 (maps:get 'riichi yaku-han 0))
                                (== (coll:get-in state open-hand)
                                    (list))))
         (next-yaku-han (map-set yaku-han 'riichi 1)))
    (if can-call-riichi?
      (tuple 'ok (coll:update-in
                  state
                  (list 'players current-player)
                  (map-set current-player-state
                           'yaku-han next-yaku-han 'stick-deposit (+ stick-deposit 1000))))
      (tuple 'ok state))))

(defaction discard (state player tile)
  "Cannot discard from your hand."
  (let* ((current-hand (list 'players current-player 'hand))
         (current-pile (list 'players current-player 'discard-pile))
         (hand (coll:get-in state current-hand))
         (tile-count (coll:mset-count hand tile))
         (next-state (clj:-> state
                             (coll:update-in current-hand (coll:mset-remove hand tile))
                             (coll:update-in current-pile (cons tile (coll:get-in state current-pile))))))
    (if (< tile-count 1)
      (tuple 'error "Cannot discard chosen tile.")
      (tuple 'ok (game:end-turn next-state)))))

;; Definition 1. Closed Hand: when you have all your pieces in your hand (14)
  ;; Winning Conditions:
    ;; 4 sets of 3 pieces each, straight or three-of-a-kind, plus a pair
    ;; 7 pairs
;; Definition 2. Open Hand: when you have some pieces in your hand AND some pieces on the table
;; in order to form kan OR chi OR pon.

(defun chii-options (hand previous-discard)
  (if (tiles:number? previous-discard)
    (let ((hand (coll:mset-add hand previous-discard))
          (spec (tile-spec previous-discard))
          (suit (tile-suit previous-discard)))
      (clj:->> spec
               (lists:seq (max (- spec 2) 1))
               (lists:foldl
                 (lambda (n acc)
                   (let* ((tile (record tile suit suit spec n))
                          (chii-option (coll:mset-normalize (yaku:get-sequence hand tile))))
                     (if (== 0 (maps:size chii-option))
                       acc
                       (cons tile acc))))
                 (list))))
    (list)))

(defun meld-action (action-name current-player state player tiles)
  (let* (((tuple tile1 tile2) tiles)
         (current-hand (list 'players current-player 'hand))
         (current-open-hand (list 'players current-player 'open-hand action-name))
         (previous-discard (list 'players
                                 (game:previous-player current-player)
                                 'discard-pile))
         (hand (coll:get-in state current-hand))
         (open-hand (coll:get-in state current-open-hand))
         (discard (coll:get-in state previous-discard))
         (meld (lists:foldl
                 (fun coll:mset-plus 2)
                 (map)
                 (list (map tile1 1)
                       (map tile2 1)
                       (map (car discard) 1))))
         (next-state (clj:-> state
                             (coll:update-in current-hand (coll:mset-minus hand (map tile1 1 tile2 1)))
                             (coll:update-in current-open-hand (cons meld open-hand))
                             (coll:update-in previous-discard (cdr discard)))))
    (map 'next-state next-state
         'discard discard
         'hand hand
         'meld meld)))

(defaction chii (state player tiles)
  "Cannot perform a chii."
  (let (((tuple tile1 tile2) tiles)
        ((map 'next-state next-state
              'discard discard
              'hand hand
              'meld meld) (meld-action 'chii current-player state player tiles)))
    (case (list (coll:mref-safe hand tile1)
                (coll:mref-safe hand tile2))
      (`(#(ok ,_) #(ok ,_))
       (if (== (chii-options meld (car discard)) '())
         (tuple 'error "Invalid chii: not a sequence")
         (tuple 'ok next-state)))
      (tile-checks
       (tuple 'error (map
                      'message "Invalid chii: tiles missing"
                      'children
                      (lists:flatmap
                       (lambda (candidate)
                         (case candidate
                           ((tuple 'ok _) (list))
                           ((tuple 'error tile) (list tile))))
                       tile-checks)))))))

(defaction pon (state player tiles)
  "Cannot perform a pon."
  (let* (((tuple tile1 tile2) tiles)
         ((map 'next-state next-state
               'discard discard
               'hand hand
               'meld meld) (meld-action 'pon current-player state player tiles))
         (discarded-tile (car discard)))
    ;; TODO: deal with red tiles
    (case (coll:mref-safe hand tile1)
      (`#(ok ,n) (when (=< 2 n) (== tile1 tile2) (== tile1 discarded-tile))
       (tuple 'ok (mset next-state 'current-player player))
      (_
       (tuple 'error (map
                      'message "Invalid pon: wrong tiles"
                      'children (list tile1 tile2 discarded-tile)))))))

;; (defun kan ())

;; closed hand win conditions
;; every set must be a flush

;; when you're one piece away from reaching that condition, you can call
;; riichi before discarding
;; after calling riichi, if you draw the tile you are missing, you earn a point

;;

                                        ; open hand win conditions

(defaction draw (state player)
  "Cannot draw"
  (let* ((current-hand (list 'players current-player 'hand))
         ((cons next-tile wall) (coll:get-in state '(wall)))
         (hand (coll:get-in state current-hand))
         (next-state (clj:-> state
                             (coll:update-in current-hand (coll:mset-add hand next-tile))
                             (coll:update-in '(wall) wall))))
    (tuple 'ok next-state)))
