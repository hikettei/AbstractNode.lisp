
(in-package :abstractnode.compiler)

(defgeneric compile-dtype (backend-indicator dtype-indicator pointer-p)
  (:documentation "
## [generic] compile-dtype
uint8_t
uint8_t *
"))


(defmethod compile-dtype ((backend-indicator t) dtype-indicator pointer-p)
  (error
   'Backend-Missing-Operation
   :op-type "compile-dtype"
   :backend backend-indicator))

