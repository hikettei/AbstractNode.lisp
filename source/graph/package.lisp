
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
  ;;(:import-from

  ;; )
  (:export
   #:AbstractTensor
   #:make-scalar
   #:make-tensor
   #:tensor-storage
   #:tensor-scalar-p
   #:tensor-shape
   #:tensor-ranges
   #:tensor-order
   #:tensor-dtype
   #:tensor-variables
   #:tensor-node
   #:tensor-memory-id)
  
  (:export
   #:Shape
   #:Shape-Exp
   ))

(in-package :abstractnode.graph)

