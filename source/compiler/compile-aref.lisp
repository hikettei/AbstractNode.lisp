
(in-package :abstractnode.compiler)

(defgeneric compile-aref (backend-indicator tensor index)
  (:documentation
   "## [generic] compile-aref

```lisp
(compile-aref backend-indicator tensor index)
```

This method is a one of compiler-components returning a form reading a certain position of the tensor.
The argument `tensor` is given as a type of `AbstractTensor`, `index` is given as a string which can be inserted directly. Given arguments, the method should generate the code which is the equivalent to the following C/Lisp code.

```clang
// Clang
ID[index]
```

```lisp
// Lisp
(aref ID index)
```

where ID = tensor.memory-id
"))

(defmethod compile-aref ((backend-indicator t) tensor index)
  (declare (ignore tensor index))
  (error
   'Backend-Missing-Operation
   :op-type "compile-aref"
   :backend backend-indicator))

(defun compile-aref-helper (backend-indicator tensor index)
  (declare (type AbstractTensor tensor))
  (apply
   #'compile-aref
   backend-indicator
   tensor
   ;; [TODO] Considering ranks and strides and broadcastings
   index))


