
(in-package :abstractnode.compiler)

(defgeneric compile-function (backend-indicator name vars dynamic-shapes body)
  (:documentation "
## [generic] compile-function

```lisp
(compile-function backend-indicator name vars dynamic-shapes body)
```

This is a one of compiler-components returning a function form which is the equivalent to:

```clang
void NAME(vars dynamic-shapes) { body }
```

If the body is given as nil, the compiler may try to generate headers; return a form without `{ body }`.

- `vars` is given as a list of `AbstractTensor`

- `dynamic-shapes` is a list of symbol used in the kernel. all symbols used here should be declared as uint32.

"))

(defmethod compile-function ((backend-indicator t) name vars dynamic-shapes body)
  (error 'Backend-Missing-Operation
	 :op-type "compile-function"
	 :backend backend-indicator))

