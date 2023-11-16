
;;
;;
;;

(in-package :abstractnode.graph)

(defstruct (Shape
	    (:constructor %make-shape (expression)))
  "## [struct] Shape
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
    node=~a, broadcast=~a
    variables=~a"
	    (tensor-shape obj)
	    (tensor-dtype obj)
	    (tensor-storage obj)
	    (tensor-memory-id obj)
	    (tensor-id obj)
	    (tensor-node obj)
	    (tensor-broadcasted-axis obj)
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


;; There's four principle operators for AbstractTensor:
;;   - reshape   : changes the shape of tensor without copying
;;   - slice     : given ranges, changes the visible area of elements
;;   - broadcast : makes stride=0 at a specified dimension.
;;   - permute   : changes the order of the axes.

(defun %apply-reshape (tensor shape-after)
  (declare (type AbstractTensor tensor)
	   (type list shape-after))

  (assert (every #'null (tensor-ranges tensor))
	  ()
	  "apply-reshape: cannot reshape a viewed tensor")

  (assert (equal
	   (tensor-order tensor)
	   (range-list 0 (length (tensor-shape tensor))))
	  ()
	  "apply-reshape: cannot reshape a tensor whose order is shuffled.")

  (let ((result (copy-tensor tensor)))
    (setf
     (tensor-broadcasted-axis result) nil
     (tensor-orig-shape result) (map 'list #'make-shape shape-after)
     (tensor-shape result)  (map 'list #'make-shape shape-after)
     (tensor-stride result) (make-tensor-stride
			     (tensor-shape result)
			     (tensor-layout result))
     (tensor-order result) (range-list 0 (length shape-after)))
    result))

(defun %apply-slice (tensor ranges)
  (declare (type AbstractTensor tensor)
	   (type list ranges))
  (assert (every #'range-p ranges)
	  nil
	  "%make-view: Ranges can be given as a list of ranges. butgot: ~a" ranges)
  (let ((result (copy-tensor tensor)))
    (setf (tensor-ranges result) ranges
	  (tensor-shape  result) (map 'list (compose #'make-shape #'range-size) ranges))
    result))

(defun %apply-broadcast (tensor nbroadcasts)
  (declare (type AbstractTensor tensor)
	   (type list nbroadcasts))
  
  (assert (and
	   (flet ((broadcast-p (x) (or (null x) (numberp x) (shape-p x))))
	     (every #'broadcast-p nbroadcasts))
	   (= (length nbroadcasts) (length (tensor-shape tensor))))
	  ()
	  "%apply-broadcast: nbroadcast can be given as: the same rank as its shape, a list of nil or fixnum/shape but got ~a" nbroadcasts)

  (let ((result (copy-tensor tensor)))
    (setf (tensor-broadcasted-axis result)
	  (map 'list (compose #'not #'null) nbroadcasts)
	  (tensor-shape result)
	  (loop for nbc   in nbroadcasts
		for shape in (tensor-shape tensor)
		collect (make-shape (or nbc shape))))
    result))

(defun %apply-permute (tensor order)
  (declare (type AbstractTensor tensor)
	   (type list order))

  (assert (= (length order)
	     (length (tensor-order tensor)))
	  ()
	  "%apply-permute: Assertion failed with (length order) = (length (tensor-order order))")
  
  (let ((result (copy-tensor tensor)))
    (macrolet ((%shuffle (slot)
		 `(setf ,slot
			(loop for index in order
			      collect
			      (nth index ,slot)))))
      (%shuffle (tensor-shape  result))
      (%shuffle (tensor-stride result))
      (%shuffle (tensor-orig-shape result))
      (%shuffle (tensor-ranges result))
      (%shuffle (tensor-broadcasted-axis result))
      ;(setf (tensor-order result) (range-list 0 (length order)))
      result)))


(defun dims (tensor) (length (tensor-shape tensor)))

