
(in-package :abstractnode.compiler)

(defgeneric compile-iteration (backend-indicator index from to by body)
  (:documentation
   "## [generic] compile-iteration

```lisp
(compile-iteration backend-indicator index from to by body)
```
An abstract form which corresponds with:
```lisp
for (uint32_t index=from; from <= to ; index++by) {
    body
}
```
"))


(defmethod compile-iteration ((backend-indicator t) index from to by body)
  (error
   'Backend-Missing-Operation
   :op-type "compile-iteration"
   :backend backend-indicator))

(defun compile-iteration-helper (backend-indicator index from to by body)
  (compile-iteration
   backend-indicator
   index
   (compile-lazy-index backend-indicator from)
   (compile-lazy-index backend-indicator to)
   (compile-lazy-index backend-indicator by)
   (compile-ir backend-indicator body)))

