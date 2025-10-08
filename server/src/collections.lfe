(defmodule collections
  (export
   (get-in 2)
   (update-nth 3)
   (update-in 3)
   (remove 2)
   (mref-safe 2)
   (mset-count 2)
   (mset-add 2)
   (mset->list 1)
   (mset-remove 2)
   (mset-minus 2)
   (mset-plus 2)
   (mset-empty 0)
   (map-indexed 2)
   (tmap 2)))

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

(defun mset-count (mset value)
  (try (map-get mset value) (catch (_ 0))))

(defun mset-add (mset value)
  (let ((count (mset-count mset value)))
    (map-set mset value (+ count 1))))

(defun mset-remove (mset value)
  (let ((count (mset-count mset value)))
    (if (< count 2)
      (map-remove mset value)
      (map-set mset value (- count 1)))))

(defun mset-minus (mset1 mset2)
  (clj:->>
    (maps:merge_with (lambda (_ l r) (- l r)) mset1 mset2)
    (maps:filter (lambda (_ v) (< 0 v)))))

(defun mset-plus (mset1 mset2)
  (maps:merge_with (lambda (_ l r) (+ l r)) mset1 mset2))

(defun mset-empty () (map))

(defun key-value->list (pair)
  (let* (((tuple tile quantity) pair))
    (fletrec ((go (key acc counter)
                  (if (== counter 0) acc (go key (cons key acc) (- counter 1)))))
      (go tile (list) quantity))))

(defun mset->list (mset)
  (clj:->> mset
           (maps:to_list)
           (lists:map (fun key-value->list 1))
           (lists:flatten)
           (lists:sort)))

(defmacro nlet
  (`(,label ,bindings . ,body)
   (when (is_atom label))
   `(fletrec ((,label ,(lists:map (fun car 1) bindings) ,@body))
      (,label ,@(lists:map (fun cadr 1) bindings)))))

(defun map-indexed (f l)
  (nlet recur ((l l)
                       (index 1))
    (case l
      (`(,elem . ,tail) (cons (funcall f elem index)
                              (recur tail (+ index 1))))
      (`() (list)))))

(defun tmap (f t)
  (clj:->> t (tuple_to_list) (map-indexed f) (list_to_tuple)))

(defun mref-safe (map key)
  (try (tuple 'ok (mref map key)) (catch (_ #(error not-found)))))
