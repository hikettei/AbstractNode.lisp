
;;
;;
;;

(in-package :abstractnode.graph)

(defstruct (Shape
	    (:constructor %make-shape (expression)))
  "## [struct] LazyAxis
"
  (exp   (make-lazyaxis expression)))

(defmethod print-object ((shape Shape) stream)
  (format stream "{Shape: ~a}" (shape-exp shape)))

(defun make-shape (expression)
  (if (shape-p expression)
      expression
      (%make-shape expression)))

(defstruct (AbstractTensor
	    (:copier copy-tensor)
	    (:conc-name tensor-))
  "## [struct] AbstractTensor
"
  (storage nil)
  (scalar-p nil         :type boolean)

  ;; [TODO] Visible-Shape/Original-Shape

  (orig-shape nil       :type Shape-T)
  (shape      nil       :type Shape-T)
  (stride     nil       :type list)
  (layout     nil)
  
  (dtype  :float        :type keyword)
  (order  nil           :type list)
  (broadcasted-axis nil :type list)
  
  (ranges nil    :type list)

  (variables nil :type list)
  (node      nil)

  (input-p  nil  :type boolean)
  (detach-p nil  :type boolean)
  ;; Memory-ID = variable name
  ;; ID = ID dedicated to topological sorting
  (memory-id (gensym "TID") :type symbol)
  (id        (gensym "ID")  :type symbol))

(defmethod print-object ((obj AbstractTensor) stream)
  (flet ((helper (tensors)
	   (format nil "~a"
		   (apply
		    #'concatenate
		    'string
		    (butlast
		     (loop for tensor in tensors
			   append
			   `(,(format
			       nil
			       "~a~a"
			       (tensor-id tensor)
			       (tensor-shape tensor))
			     " ")))))))

    (format stream "AbstractTensor{~a, ~a}
    storage=~a
    memory-id=~a, id=~a
    node=~a
    variables=~a"
	    (tensor-shape obj)
	    (tensor-dtype obj)
	    (tensor-storage obj)
	    (tensor-memory-id obj)
	    (tensor-id obj)
	    (tensor-node obj)
	    (helper (tensor-variables obj)))))

(defun make-scalar (storage dtype
		    &key
		      (memory-id (gensym "TID"))
		      (variables nil))
  (make-AbstractTensor
   :storage storage
   :scalar-p T
   :dtype dtype
   :variables variables
   :memory-id memory-id))

(defun make-tensor (shape dtype
		    &key
		      (input-p nil)
		      (layout :row)
		      (order nil)
		      (memory-id (gensym "TID"))
		      (variables nil)
		      (broadcasted-axis nil))

  (assert (member layout `(:row :column))
	  ()
	  "make-tensor: layout should be given as one of: :row :column. butgot ~a" layout)
  
  (make-AbstractTensor
   :input-p input-p
   :storage nil
   :layout layout
   :stride     (make-tensor-stride shape layout)
   :orig-shape (map 'list #'make-shape shape)
   :shape      (map 'list #'make-shape shape)
   :dtype      dtype
   :order      (or order (range-list 0 (length shape) 1))
   :broadcasted-axis broadcasted-axis
   :ranges  nil
   :variables variables
   :memory-id memory-id))

