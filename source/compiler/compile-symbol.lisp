
(in-package :abstractnode.compiler)

(defgeneric compile-symbol (backend-indicator symbol)
  (:documentation
   "## [generic] compile-symbol

```lisp
(compile-symbol backend-indicator symbol)
```

This method is a one of compile-components translating `symbol` into a name of variables in a certain backend.

e.g.: In clang, including `-` in a variable name is prohibited while it is legal in lisp languages.

so the symbol `batch-size` needs to be translated into: `batch_size`.
"))

(defmethod compile-symbol ((backend-indicator t) symbol)
  (error
   'Backend-Missing-Operation
   :op-type "compile-symbol"
   :backend backend-indicator))

(defmethod compile-symbol :around (backend-indicator symbol)
  (if (symbolp symbol)
      (call-next-method)
      symbol))


