
(in-package :abstractnode.compiler)

(defgeneric compile-requirements (backend-indicator)
  (:documentation
   "## [generic] compile-requirements"))

(defmethod compile-requirements ((backend-indicator t))
  (error
   'Backend-Missing-Operation
   :op-type "compile-requirements"
   :backend backend-indicator))

