(defmodule xml
  (export (read-one-element 1)
          (write-game 2)
          (read-action-params 2)
          (serialize-event 2))
  (module-alias (collections coll)))

(defun read-action-params
  ((`#(discard #m(suit ,suit spec ,spec) ()) player-id)
   (let* ((suit (erlang:list_to_atom suit))
          (spec (if (lists:member suit (tiles:numbered-suits))
                  (erlang:list_to_integer spec)
                  (erlang:list_to_atom spec))))
     (tuple 'discard (map 'tile (tuple 'tile suit spec)
                          'player player-id))))
  ((`#(draw #m() ()) player-id)
   (tuple 'draw (map 'player player-id)))
  ((`#(riichi #m() ()) player-id)
   (tuple 'riichi (map 'player player-id)))
  ((unknown _) (tuple 'error unknown)))

(defun attributes (node)
  (lists:foldl
   (lambda (attribute acc)
     (let ((name (tref attribute 2))
           (value (tref attribute 9)))
       (mset acc name (read-one-element value))))
   (map)
   (xmerl_xs:select "@*" node)))

(defun one-element* (node)
  (case (xmerl_xs:select "name()" node)
    (`#(xmlObj string ,tag)
     (tuple (erlang:list_to_atom tag)
       (attributes node)
       (lists:map (fun one-element* 1) (tref node 9))))))

(defun read-one-element (serialized-element)
  (try
    (let ((node (clj:-> serialized-element
                  (unicode:characters_to_list 'utf8)
                  (xmerl_scan:string)
                  (tref 1))))
      (one-element* node))
    (catch (`#(,type ,value ,stacktrace)
            serialized-element))))

(defun serialize-spec (tile spec)
  (if (tiles:number? tile)
    (erlang:integer_to_list spec)
    spec))

(defun convert-tile
  (((= tile (tuple 'tile suit spec)))
   (tuple 'tile (list (tuple 'suit suit) (tuple 'spec (serialize-spec tile spec))) '())))

(defun convert-single-open-hand (list-melds)
  (lists:map
   (lambda (t)
     (let (((tuple tag list-tiles) t))
       (tuple tag '() (lists:map (fun convert-tile 1) list-tiles))))
   list-melds))

(defun convert-open-hand (list-melds)
  (tuple 'open-hand '() (convert-single-open-hand list-melds)))

(defun convert-pile (tag pile)
  (tuple tag '() (lists:map (fun convert-tile 1) pile)))

(defun yaku-han-entry->tuple
  (((tuple yaku quantity))
   (tuple yaku (integer_to_list quantity))))

(defun convert-yaku-han (yaku-han)
  (tuple 'yakus (clj:->> yaku-han
                  (maps:to_list)
                  (lists:map (fun yaku-han-entry->tuple 1)))
         '()))

(defun convert-full-player
  (((map 'hand hand 'discard-pile discard-pile 'open-hand open-hand 'yaku-han yaku-han 'stick-deposit stick-deposit))
   (list
     (convert-pile 'hand (coll:mset->list hand))
     (convert-pile 'discard-pile discard-pile)
     (convert-open-hand open-hand)
     (convert-yaku-han yaku-han)
     (tuple 'stick-deposit (list (tuple 'quantity (erlang:integer_to_list stick-deposit))) '()))))

(defun convert-player (player-number)
  (lambda (player index)
    (tuple 'player
           (if (== index player-number)
             (convert-full-player player)
             (lists:foldl
               (lambda (info acc)
                 (let ((value (map-get player info)))
                   (case info
                     ('discard-pile (cons (convert-pile 'discard-pile value) acc))
                     ('open-hand (cons (convert-open-hand value) acc)))))
               (list)
               (game:public-information))))))

(defun write-game (player-number player-state)
  (let ((players (clj:->> 'players
                   (map-get player-state)
                   (coll:tmap (convert-player player-number))
                   (tuple_to_list)))
        (current-player (map-get player-state 'current-player)))
    (xmerl:export_simple (list (tuple 'game
                                      (list (tuple 'you (erlang:integer_to_list player-number))
                                            (tuple 'current-player (erlang:integer_to_list current-player)))
                                      (list (tuple 'players players)))) 'xmerl_xml)))

(defun write-event (event mapp)
  (clj:-> event
          (tuple (maps:to_list mapp) '())
          (list)
          (xmerl:export_simple 'xmerl_xml)))

(defun serialize-event
  (('play (tuple player-number gamestate))
   (map 'data (write-game player-number gamestate)))
  ((event data)
   (clj:->> data (write-event event) (map 'data))))
