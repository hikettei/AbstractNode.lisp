
# AbstractNode.lisp

A Tiny and Portable DAG Compiler intended to be integrated into cl-waffe2.

## Goals

- Eazy to Interop with other languages via shared lib (e.g.: Implementing Python frontend using AbstractNode, from AbstractNode to shared library dedicated to the model.)

- 100% JIT Compiler Based Matrix Operations (and ANSI Common Lisp)

    - (Once the package is loaded, it also jit-compiles element-wise operations declared. so abstractnode.lisp is intended to be used numpy-like library)

    - We gonna provide a macro declaring the template of jit-compiled backend.

- Dynamic Shape, LazyAxis, Control Flow, Multiple Backends with small codes

- Basically it intends to provide a low-level API that can be used by elegant frontends like cl-waffe2.

- No dependencies, works anywhere, eazy to write an extension

```lisp
[cl-waffe             (Elegant Frontend)]
                 |
[AbstractNode.lisp (A Low-Level Backend)]
                 |
        [abstractnode.graph]
	[abstractnode.compiler]
```

```lisp
(define-backend :GCC
    :compiler "gcc"
    :iterator ...
    :header-template "void ..."
    :arithmetic `("+" "-" "*" "/")
    :mathematical-ops
        `((:sin "sin")
         ...))
```

