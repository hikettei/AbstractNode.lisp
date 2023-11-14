
(in-package :abstractnode.apis)

;; [TODO] Numpy-Semantic Broadcasting

(defun element-wise (&rest args)
  "Element-Wise Operation is defined as:
- 1. Reads N-length arguments
- 2. Overwrites the result into the first one."

  ;; [TODO] Asserting all arguments have the same shape
  (values
   (make-tensor (tensor-shape (car args))
		(tensor-dtype (car args))
		:order  (tensor-order  (car args))
		:id     (tensor-id     (car args)))))

(export `(lazy-copy lazy-move))
(defun lazy-move (move-to move-from &key (force nil))
  (call
   (make-op :=
	    :name 'lazy-move
	    :next-outputs #'element-wise
	    :in-place-mutation-p (not force))
   move-to
   move-from))

(defun lazy-copy (tensor &key (force nil))
  (lazy-move
   (make-tensor (tensor-shape tensor)
		(tensor-dtype tensor)
		:order (tensor-order tensor)
		:input-p (tensor-input-p tensor))
   tensor
   :force force))

(macrolet ((def (name node-named op)
	     `(progn
		(export ',name)
		(defun ,name (&rest args)
		  (apply
		   #'call
		   (make-op
		    ,op
		    :next-outputs #'element-wise
		    :name ',node-named)
		   `(,(lazy-copy (car args)) ,@(cdr args)))))))
  (def lazy-add lazy-add :+)
  (def lazy-sub lazy-sub :-)
  (def lazy-mul lazy-mul :*)
  (def lazy-div lazy-div :/))
