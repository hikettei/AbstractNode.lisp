
(in-package :abstractnode.apis)

(defun parse-absolute (total abs)
  (if (>= abs 0)
      abs
      (+ total abs)))

(export 'lazy-permute)
(defun lazy-permute (tensor order)
  (call
   (make-op :permute
	    :name 'permute
	    :system-ir-p t
	    :next-outputs #'element-wise)
   (%apply-permute tensor order)
   tensor))

(export 'lazy-reshape)
(defun lazy-reshape (tensor new-shape)
  (call
   (make-op :permute
	    :name 'reshape
	    :system-ir-p t
	    :next-outputs #'element-wise)	  
   (%apply-reshape tensor new-shape)
   tensor))

(export 'lazy-broadcast)
(defun lazy-broadcast (tensor broadcast-to)
  (call
   (make-op :broadcast
	    :name 'broadcast
	    :system-ir-p t
	    :next-outputs #'element-wise)	  
   (%apply-broadcast tensor broadcast-to)
   tensor))

(export 'lazy-slice)
(defun lazy-slice (tensor ranges)
  (call
   (make-op :slice
	    :name 'slice
	    :system-ir-p t
	    :next-outputs #'element-wise)
   (%apply-slice tensor ranges)
   tensor))

