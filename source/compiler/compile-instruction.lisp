
(in-package :abstractnode.compiler)

(defgeneric compile-instruction (backend-indicator op &rest args)
  (:documentation
   "
## [generic] compile-instruction

```lisp
(compile-instruction backend-indicator op &rest args)
```

The method is a one of compiler-components translating `op` into the string for a certain backend.

- `op` is given as a keyword which is used when making a shape/AbstractNode

- `args` is a list of string that can be inserted directly

"))


(defmethod compile-instruction ((backend-indicator t) (op t) &rest args)
  (declare (ignore args))
  (error
   'Backend-Missing-Operation
   :op-type (format nil "compile-instruction (where op=~a)" op)
   :backend backend-indicator))

(macrolet ((def (op lisp-op)
	     `(defmethod compile-instruction
		  :around
		  ((backend-indicator t)
		   (op (eql ,op))
		   &rest args)
		(if (every #'numberp args)
		    (apply ,lisp-op args)
		    (call-next-method)))))
  ;; With regard to following instructions,
  ;; the compiler try to compute the result in advance as long as all arguments are scalar numbers
  ;; e.g.: (:+ 1 1) -> 2
  (def :+ #'+)
  (def :- #'-)
  (def :* #'*)
  (def :/ #'/)

  (def :max #'max)
  (def :min #'min)
  
  )
