
;;
;;
;;

(in-package :abstractnode.graph)

(defstruct (Shape
	    (:constructor %make-shape (expression)))
  "## [struct] LazyAxis
"
  (exp   (make-lazyaxis expression) :type (or fixnum list symbol LazyAxis)))

(defmethod print-object ((shape Shape) stream)
  (format stream "{Shape: ~a}" (shape-exp shape)))

(defun make-shape (expression)
  (if (shape-p expression)
      expression
      (%make-shape expression)))

(defstruct (AbstractTensor
	    (:conc-name tensor-))
  "## [struct] AbstractTensor
"
  (storage nil)
  (scalar-p t    :type boolean)

  (shape  nil    :type Shape-T)
  (dtype  :float :type keyword)
  (order  nil    :type list)
  (ranges nil    :type list)

  (variables nil :type list)
  (node      nil)
  ;; Memory-ID = variable name
  ;; ID = ID dedicated to topological sorting
  (memory-id (gensym "TID") :type symbol)
  (id        (gensym "ID")  :type symbol))

(defun make-scalar (storage dtype
		    &key
		      (id (gensym "TID"))
		      (variables nil))
  (make-AbstractTensor
   :storage storage
   :scalar-p T
   :dtype dtype
   :variables variables
   :memory-id id))

(defun make-tensor (shape dtype
		    &key
		      (layout :row)
		      (order nil)
		      (id (gensym "TID"))
		      (variables nil))

  (assert (member layout `(:row :column))
	  ()
	  "make-tensor: layout should be given as one of: :row :column. butgot ~a" layout)
  
  (make-AbstractTensor
   :storage nil
   :shape   (map 'list #'make-shape shape)
   :dtype   dtype
   :order   (or order
		(ecase layout
		  (:row
		   (range-list 0 (length shape) 1))
		  (:column
		   (range-list (length shape) 0 1))))
   :ranges  nil
   :variables variables
   :memory-id id))

;; todo: implementing arange by myself
;; Index-Components allowing to access the current position in the iteration
;; Arange can be implemented like: Empty-Array <- Index-Components

