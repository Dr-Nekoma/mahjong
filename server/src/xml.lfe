(defmodule xml
  (export (one-element 1)
	  (play-action-params 1)))

(defun play-action-params
  ((`#m(player-id ,player-id action #(discard #m(suit ,suit spec ,spec))))
   (let ((suit (erlang:list_to_atom suit))
	 (spec (if (lists:member suit (tiles:numbered-suits))
		 (erlang:list_to_integer spec)
		 (erlang:list_to_atom spec))))
     (tuple 'discard (map 'tile (tuple 'tile suit spec)
			  'player (list_to_integer player-id)))))
  ((`#m(player-id ,player-id action #(draw #m())))
   (tuple 'draw (map 'player (list_to_integer player-id))))
  ((`#m(player-id ,player-id action #(riichi #m())))
   (tuple 'riichi (map 'player (list_to_integer player-id)))))

(defun one-element (serialized-element)
  (try
    (let ((node (clj:-> serialized-element
                (unicode:characters_to_list 'utf8)
                (xmerl_scan:string)
                (tref 1))))
      (case (xmerl_xs:select "name()" node)
        (`#(xmlObj string ,tag)
         (tuple (erlang:list_to_atom tag)
                (lists:foldl
                 (lambda (attribute acc)
                   (let ((name (tref attribute 2))
                         (value (tref attribute 9)))
                     (mset acc name (one-element value))))
                 (map)
                 (xmerl_xs:select "@*" node))))))
    (catch (`#(,type ,value ,stacktrace)
            serialized-element))))
