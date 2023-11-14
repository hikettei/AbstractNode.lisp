
(cl:in-package :cl-user)

(defpackage :abstractnode.compiler
  (:documentation
   "This package provides an abstract compiler dedicated to matrix operation.
Operations are consisted of two factors:
   [Iterator] {
      [Instruction]
   }

User-extended backends are have to follow instruction given this package.

Overall, backends must write codes like:
[Requirements (e.g.: include)]
[Function Declarations (e.g.: headers)]
[Operation Implementations (e.g.: for ...)]

This compilation isn't necessary lazily; users can compile all operations first.

In cl-waffe2, ALL MODELS ARE REPRESENTED BY THESE COMPONENTS:
   - [method] compile-requirements
   - [method] compile-function-headers
   - [method] compile-lazy-index
   - [method] compile-aref
   - [method] compile-iteration
   - [method] compile-instruction
")
  (:use :cl)
  (:import-from
   :abstractnode.graph
   #:AbstractTensor
   )
  
  (:export
   #:Backend-Missing-Operation)
  
  (:export
   #:compile-lazy-index
   #:compile-aref
   #:compile-requirements
   #:compile-dtype
   ))

