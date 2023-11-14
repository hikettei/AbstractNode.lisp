
(cl:in-package :cl-user)

(defpackage :abstractnode.lisp-backend
  (:use :cl :AbstractNode.graph :AbstractNode.compiler))

(in-package :abstractnode.lisp-backend)

(defmethod compile-requirements ((backend-indicator (eql :lisp))) "")

(defmethod compile-lazy-index ((backend-indicator (eql :lisp)) shape)
  (format nil "~a" shape))

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
    by ~a do ~a)" index from to by body))

(print (compile-iteration-helper
	:lisp
	"A"
	0
	10
	2
	"(+ 1 1)"))

(print (abop:lazy-add
	(make-tensor `(3 3) :float)
	(make-tensor `(3 3) :float)))

