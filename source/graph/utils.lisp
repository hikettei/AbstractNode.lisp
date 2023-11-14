
(in-package :abstractnode.graph)

(defun all-shape-p (obj) (every #'shape-p obj))
(deftype shape-t ()
  "A Type Indicating the shape of tensors"
  `(and list (satisfies all-shape-p)))


(declaim (inline range-list))
(defun range-list (&optional (from 0) (to (1+ from)) (by 1))
  (loop for i upfrom from below to by by collect i))
  
