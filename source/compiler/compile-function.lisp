
(in-package :abstractnode.compiler)

(defgeneric compile-function (backend-indicator name vars dynamic-shapes body)
  (:documentation "
void NAME(vars dynamic-shapes) { body }
If body is set to nil -> remove {} "))

(defmethod compile-function ((backend-indicator t) name vars dynamic-shapes body)
  (error 'Backend-Missing-Operation
	 :op-type "compile-function"
	 :backend backend-indicator))

