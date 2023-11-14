
(cl:in-package :cl-user)

(defpackage :abstractnode.lisp-backend
  (:use :cl :AbstractNode.graph :AbstractNode.compiler))

(in-package :abstractnode.lisp-backend)

(defmethod compile-requirements ((backend-indicator (eql :lisp))) "")

(defmethod compile-lazy-index ((backend-indicator (eql :lisp)) shape)
  (if (shape-p shape)
      (format nil "~a" (shape-exp shape))
      (format nil "~a" shape)))

(defmethod compile-aref ((backend-indicator (eql :lisp)) tensor &rest subscripts)
  (format nil "(aref ~a ~a)"
	  (tensor-memory-id tensor)
	  (apply
	   #'concatenate
	   'string
	   (butlast
	    (loop for s in subscripts
		  append
		  `(,s " "))))))

(defmethod compile-dtype ((backend-indicator (eql :lisp)) dtype pointer-p)
  (symbol-macrolet ((dtype-helper 
		      (ecase dtype
			(:uint32
			 "(unsigned-byte 32)")
			(:double
			 "double-float")
			(:float
			 "single-float"))))
    (if pointer-p
	(format nil "(simple-array ~a (*))" dtype-helper)
	dtype-helper)))

(defmethod compile-iteration ((backend-indicator (eql :lisp))
			      index
			      from
			      to
			      by
			      body)
  (format nil "(loop for ~a of-type (unsigned-byte 64)
    upfrom ~a
    below ~a
    by ~a do~% ~a)" index from to by body))

(defmethod compile-function ((backend-indicator (eql :lisp))
			     name
			     vars
			     dynamic-shapes
			     body)
  (when (null body) (return-from compile-function ""))
  (with-output-to-string (out)
    (format out "(defun ~a ("  name)
    (loop for tensor in vars do
      (format out "~a " (tensor-id tensor)))

    (loop for shape  in dynamic-shapes do
      (format out "~a " shape))
    (format out ")")
    (format out "~%(declare~%(optimize (speed 3))")    
    (loop for tensor in vars do
      (format out "~%(type ~a ~a)"
	      (compile-dtype backend-indicator
			     (tensor-dtype tensor)
			     (not (tensor-scalar-p tensor)))
	      (tensor-id tensor)))
    (loop for shape  in dynamic-shapes do
      (format out "~%(type ~a ~a)"
	      :uint32
	      shape))
    (format out ")")
    (format out "~%~a)" body)))

(print
 (time
  (compile-with-backend
   :lisp
   (abop:lazy-add    
    (abop:lazy-mul
     (make-tensor `(3 3 3) :float :input-p t)
     (make-tensor `(3 3 3) :float))
    (make-tensor `(3 3 3) :float)))))

