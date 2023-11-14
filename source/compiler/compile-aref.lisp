
(in-package :abstractnode.compiler)

(defgeneric compile-aref (backend-indicator tensor &rest subscripts)
  (:documentation
   "## [generic] compile-aref
Can be read as: tensor.memory-id[subscripts[a list of symbol/fixnum]]
"))

(defmethod compile-aref ((backend-indicator t) tensor &rest subscripts)
  (declare (ignore tensor subscripts))
  (error
   'Backend-Missing-Operation
   :op-type "compile-aref"
   :backend backend-indicator))

(defun compile-aref-helper (backend-indicator tensor &rest subscripts)
  (declare (type AbstractTensor tensor))
  (apply
   #'compile-aref
   backend-indicator
   tensor
   (map
    'list
    #'(lambda (shape)
	(compile-lazy-index backend-indicator shape))
    subscripts)))


