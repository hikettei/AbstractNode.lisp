
(in-package :abstractnode.compiler)

(defgeneric compile-lazy-index (backend-indicator shape)
  (:documentation
   "## [generic] compile-lazy-index
Implements a simple compiler which compilers from abstractnode.graph:Shape to string.
"))

(defmethod compile-lazy-index ((backend-indicator t) shape)
  (error
   'Backend-Missing-Operation
   :op-type "compile-lazy-index"
   :backend backend-indicator))

