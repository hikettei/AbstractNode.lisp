
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
		   args)))))
  (def lazy-add lazy-add :+)
  (def lazy-sub lazy-sub :-)
  (def lazy-mul lazy-mul :*)
  (def lazy-div lazy-div :/))
