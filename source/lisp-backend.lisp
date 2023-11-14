
(cl:in-package :cl-user)

(defpackage :abstractnode.lisp-backend
  (:use :cl :AbstractNode.graph :AbstractNode.compiler))

(in-package :abstractnode.lisp-backend)

(defmethod compile-requirements ((backend-indicator (eql :lisp))) "")

(defmethod compile-lazy-index ((backend-indicator (eql :lisp)) shape)
  (format nil "~a" shape))

(defmethod compile-aref ((backend-indicator (eql :lisp)) tensor &rest subscripts)
  (with-output-to-string (out)
    (format out "(aref ~a " (tensor-memory-id tensor))
    (dolist (s subscripts)
      (format out "~a " s))
    (format out ")")))

(defmethod compile-dtype ((backend-indicator (eql :lisp)) dtype)
  (ecase dtype
    (:double
     "double-float")
    (:float
     "single-float")))

(print (abstractnode.compiler::compile-aref-helper
	:lisp
	(make-tensor `(3 3) :float)
	2 1))
