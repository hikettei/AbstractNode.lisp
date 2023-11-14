
(cl:in-package :cl-user)

;; [TODO] SLEEF (Loop Collapse and ...), Vectorized Math

(defpackage :abstractnode.gcc-backend
  (:use :cl :AbstractNode.graph :AbstractNode.compiler))

(in-package :abstractnode.gcc-backend)

(defmethod compile-requirements ((backend-indicator (eql :gcc))) "")

(defmethod compile-lazy-index ((backend-indicator (eql :gcc)) shape)
  "")

(defmethod compile-aref ((backend-indicator (eql :gcc)) tensor &rest subscripts)
  "")
