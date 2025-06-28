(defmodule collections
  (export
   (get-in 2)
   (update-nth 3)
   (update-in 3)
   (remove 2)))

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
