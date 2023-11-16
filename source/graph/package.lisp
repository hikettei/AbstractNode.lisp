
(cl:in-package :cl-user)

(defpackage :abstractnode.graph
  (:documentation
   "
## [Package] AbstractNode.Graph

A Package dedicated to express the computation node comprised of two principle objects:
 - AbstractTensor -> A Struct representing variables used in the network.
 - AbstractNode   -> A Struct representing operations in the network.
")
  (:use :cl)
  (:export
   #:call
   #:make-op

   #:AbstractNode-id
   #:AbstractNode
   #:AbstractNode-name
   #:AbstractNode-op
   #:AbstractNode-in-args
   #:AbstractNode-out-args
   #:AbstractNode-system-ir-p
   #:AbstractNode-In-Place-Mutation-P
   #:AbstractNode-Next-Outputs)

  (:export
   #:do-ranked-tensor
   #:AbstractTensor
   #:copy-tensor
   #:make-scalar
   #:make-tensor
   #:dims
   #:tensor-storage
   #:tensor-scalar-p
   #:tensor-shape
   #:tensor-stride
   #:tensor-layout
   #:tensor-ranges
   #:tensor-order
   #:tensor-dtype
   #:tensor-variables
   #:tensor-node
   #:tensor-memory-id
   #:tensor-input-p
   #:tensor-id
   #:tensor-broadcasted-axis
   #:tensor-orig-shape
   #:tensor-detach-p)
  
  (:export
   #:Shape-p
   #:make-shape
   #:Shape
   #:Shape-Exp)

  (:export
   #:range
   #:range-list)

  (:export
   #:%apply-reshape
   #:%apply-slice
   #:%apply-broadcast
   #:%apply-permute))

(in-package :abstractnode.graph)
