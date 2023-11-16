
(in-package :abstractnode.compiler)


(defgeneric compile-endline (backend-indicator)
  (:documentation "
## [generic] compile-endline

```lisp
(compile-endline backend-indicator)
```

This is a one of compiler-components (Optional).

(TODO: Specs)
"))

(defmethod compile-endline ((backend-indicator t))
  (if (next-method-p)
      (call-next-method)
      ""))

