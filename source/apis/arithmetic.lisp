
(in-package :abstractnode.apis)

;; A list of fundamental ops:
;; :+ :- :* :/ :=
;; :dcast
;;

;; A list of mathematical ops:
;;  :sin :cos :tan ...

(defun element-wise (&rest args)
  "Element-Wise Operation is defined as:
- 1. Reads N-length arguments
- 2. Overwrites the result into the first one."

  ;; [TODO] Asserting all arguments have the same shape/dtype
  (values
   (make-tensor (tensor-shape (car args))
		(tensor-dtype (car args))
		:layout    (tensor-layout (car args))
		:order     (tensor-order  (car args))
		:memory-id (tensor-memory-id     (car args)))))

(export `(lazy-copy lazy-move))
(defun lazy-move (move-to move-from &key (force nil))
  (with-broadcasted-arrays (move-to move-from)
    (call
     (make-op :=
	      :name 'lazy-move
	      :next-outputs #'element-wise
	      :in-place-mutation-p (not force))
     move-to
     move-from)))

(defun lazy-copy (tensor &key (force nil))
  (lazy-move
   (make-tensor (tensor-shape tensor)
		(tensor-dtype tensor)
		:layout  (tensor-layout tensor)
		:order   (tensor-order tensor)
		:input-p (tensor-input-p tensor))
   tensor
   :force force))

(macrolet ((def (name node-named op)
	     `(progn
		(export ',name)
		(defun ,name (&rest args)
		  (with-broadcasted-arrays args
		    (apply
		     #'call
		     (make-op
		      ,op
		      :next-outputs #'element-wise
		      :name ',node-named)
		     `(,(lazy-copy (car args)) ,@(cdr args))))))))
  (def lazy-add lazy-add :+)
  (def lazy-sub lazy-sub :-)
  (def lazy-mul lazy-mul :*)
  (def lazy-div lazy-div :/))

