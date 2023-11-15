
(in-package :abstractnode.compiler)

(defgeneric compile-iteration (backend-indicator index from to by body)
  (:documentation
   "## [generic] compile-iteration

```lisp
(compile-iteration backend-indicator index from to by body)
```

The method compile-iteration is a one of compiler-components generating iterations.

The method should return the string for a certain backend, which is the equivalent to the following C code:
```lisp
for (uint32_t `index`=`from`; `index` <= `to` ; `index`++`by`) {
    `body`
}
```
where `index`, `from`, `to`, `by`, and `body` are the arguments given by the method which is already compiled so that the string which can be inserted directly.
"))


(defmethod compile-iteration ((backend-indicator t) index from to by body)
  (error
   'Backend-Missing-Operation
   :op-type "compile-iteration"
   :backend backend-indicator))

(defun compile-iteration-helper (backend-indicator index from to by body)
  (compile-iteration
   backend-indicator
   (compile-symbol backend-indicator index)
   (compile-lazy-index backend-indicator (make-shape from))
   (compile-lazy-index backend-indicator (make-shape to))
   (compile-lazy-index backend-indicator (make-shape by))
   body))

