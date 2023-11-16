
# AbstractNode.lisp

A Tiny and Portable DAG Compiler which is intended to be integrated into cl-waffe2.

Visit the `./source/lisp-backend.lisp` file to understand the goal of this project.

## Goals

- Eazy to Interop with other languages via shared lib (e.g.: Implementing Python frontend using AbstractNode, from AbstractNode to shared library dedicated to the model.)

- 100% JIT Compiler Based Matrix Operations (and ANSI Common Lisp)

    - (Once the package is loaded, it also jit-compiles element-wise operations declared. so abstractnode.lisp is intended to be used numpy-like library)

    - We gonna provide a macro declaring the template of jit-compiled backend.

- Dynamic Shape, LazyAxis, Control Flow, Multiple Backends with small codes

- Basically it intends to provide a low-level API that can be used by elegant frontends like cl-waffe2.

- No dependencies, works anywhere, eazy to write an extension

- Multi-Threading Scheduling/Reordering/Performance Turning/In-place mutation

```lisp
[cl-waffe             (Elegant Frontend)]
                 |
[AbstractNode.lisp (A Low-Level Backend)]
                 |
        [abstractnode.graph]
	[abstractnode.compiler]
```

When reimplementing backends to another accelerator, all users have to do is to implement these methods:

```lisp
- [method] compile-requirements
- [method] compile-function-headers
- [method] compile-lazy-index
- [method] compile-iterator
- [method] compile-instruction
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

AbstractTensor = Basically regarded as a 1D Array but multiple-dimensional

# References

# Workload

- [ ] Constructing a solid foundation
- [ ] No dependencies (as possible)
- [ ] Polyhedral Compiler, Parallelizing, Auto tuning, device-specific optimizations.
- [ ] Implementing a cross compiler from AbstractNode.lisp to CLANG, Metal, OpenCL, and CUDA!
- [ ] CLI Tools (./roswell/abstractnode-cli.ros, gcc-like features)
- [ ] Autodiff (Lens)
- [ ] CNN/RNN/Transformer Inference

