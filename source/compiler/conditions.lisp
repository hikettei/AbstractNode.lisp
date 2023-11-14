
(in-package :abstractnode.compiler)

(define-condition Backend-Missing-Operation ()
  ((backend :initarg :backend)
   (op-type :initarg :op-type :type string))
  (:report
   (lambda (condition stream)
     (format
      stream
      "AbstractNode failed to compile the instruction ~a because the backend ~a didn't provide any implementation of it.~%"
      (slot-value condition 'op-type)
      (slot-value condition 'backend)))))
