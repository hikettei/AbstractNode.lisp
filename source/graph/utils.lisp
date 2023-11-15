
(in-package :abstractnode.graph)

(defun all-shape-p (obj) (every #'shape-p obj))
(deftype shape-t ()
  "A Type Indicating the shape of tensors"
  `(and list (satisfies all-shape-p)))

(declaim (inline range-list))
(defun range-list (&optional (from 0) (to (1+ from)) (by 1))
  (loop for i upfrom from below to by by collect i))

(defmacro do-ranked-tensor ((bind-rank orig-size visible-size broadcast-p range) tensor &body body)
  "
## [macro] do-ranked-tensor

Iterates shape over a rank following the order of `tensor-order`.

"
  `(progn
     (assert (not (tensor-scalar-p ,tensor))
	     ()
	     "do-ranked-tensor: Assertion failed because the tensor ~a shouldn't be a scalar." ,tensor)
     (dolist (,bind-rank (tensor-order ,tensor))
       (let ((,orig-size    (nth ,bind-rank (tensor-orig-shape ,tensor)))
	     (,visible-size (nth ,bind-rank (tensor-shape ,tensor)))
	     (,broadcast-p  (nth ,bind-rank (tensor-broadcasted-axis ,tensor)))
	     (,range        (nth ,bind-rank  (tensor-ranges ,tensor))))
	 (declare (ignorable ,orig-size ,visible-size ,broadcast-p ,range))
	 ,@body))))

