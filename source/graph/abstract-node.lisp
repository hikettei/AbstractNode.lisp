
;; Abstract-Tensor.lisp provides features for AbstractTensor.
;; Formulating lambda expression based AST
;; Multiple-Backends: Once the code has written for CPU, it should also works on GPU/Metal/OpenCL without changing codes

(in-package :abstractnode.graph)

(defstruct (AbstractNode)
  "
## [struct] AbstractNode
out[0], out[1], ... <- op(in[0], in[1], ...)
where op is named as `name`"
  (id   (gensym "NID")  :type symbol)
  (name (gensym "NODE") :type symbol)
  (op)
  
  (in-args  nil :type list)
  (out-args nil :type list)
  
  (system-ir-p nil :type boolean) ;; If Set to T, the node will never appeares in the compiled graph.
  (in-place-mutation-p nil :type boolean)
  
  (next-outputs #'(lambda (&rest tensors) tensors) :type function))

(defmethod print-object ((obj AbstractNode) stream)
  (flet ((helper (tensors)
	   (format nil "~a"
		   (apply
		    #'concatenate
		    'string
		    (butlast
		     (loop for tensor in tensors
			   append
			   `(,(format
			       nil
			       "~a~a"
			       (tensor-memory-id tensor)
			       (tensor-shape tensor))
			     " ")))))))
    
    (format stream "AbstractNode{~a~a~a} ~a <- ~a(~a)"
	    (if (abstractnode-system-ir-p obj)
		", system_ir=t"
		"")
	    (if (abstractnode-in-place-mutation-p obj)
		", in_place_mutation_p=t"
		"")
	    (abstractnode-name obj)
	    (helper
	     (abstractnode-out-args obj))
	    (abstractnode-op   obj)
	    (helper
	     (abstractnode-in-args  obj)))))

(defun make-op (op
		&key
		  (name nil)
		  (next-outputs #'(lambda (&rest tensors) tensors))
		  (system-ir-p nil)
		  (in-place-mutation-p nil))
  (make-AbstractNode
   :name (or name (gensym "NODE"))
   :op op
   :next-outputs next-outputs
   :system-ir-p system-ir-p
   :in-place-mutation-p in-place-mutation-p))

(defun call (AbstractNode &rest tensors)
  "
## [function] call
"
  (declare (type AbstractNode abstractnode)
	   (type list tensors))

  (let ((next-outputs
	  (multiple-value-list
	   (apply
	    (AbstractNode-next-outputs abstractnode)
	    tensors))))
    ;; Recording Computation Nodes
    (mapc
     #'(lambda (out-tensor)
	 (setf (tensor-variables out-tensor) tensors
	       (tensor-node      out-tensor) AbstractNode))
     next-outputs)


    (setf (AbstractNode-in-args  AbstractNode) tensors
	  (AbstractNode-out-args AbstractNode) next-outputs)

    (apply #'values next-outputs)))

