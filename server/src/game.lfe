(defmodule game
  ;; app implementation
  (export
   (play 1)))


(defun play (state)
  (receive
    (_ (progn
	 (io:format "I've passed in here buddy xD!")
	 (play state)))))
