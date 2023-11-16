
(cl:in-package :cl-user)

(defpackage :abstractnode.compiler
  (:documentation
   "
## [package] abstractnode.compiler

This package provides an user-extensible compiler dedicated to matrix operations.

To extend AbstractNode compiler to any devices you like, all users have to do is to extend these methods (we call it compiler-components).
   - [method] compile-requirements
   - [method] compile-dtype
   - [method] compile-symbol
   - [method] compile-aref
   - [method] compile-function
   - [method] compile-instruction
   - [method] compile-iteration
   - [method] compile-endline (Optional)
   
")
  (:use :cl :AbstractNode.graph)

  ;; Conditions
  (:export
   #:Backend-Missing-Operation)

  (:export
   #:compile-with-backend
   #:compute-with-backend)  
  
  (:export
   #:compile-symbol
   #:compile-instruction
   #:compile-lazy-index
   #:compile-aref
   #:compile-aref-helper
   #:compile-requirements
   #:compile-dtype
   #:compile-iteration
   #:compile-iteration-helper
   #:compile-function
   #:compile-endline
   ))

