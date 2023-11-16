
(in-package :abstractnode.apis)

(export 'lazy-reduce)
(defun lazy-reduce (op tensor dim
		    &aux
		      (dim
		       (flet ((helper (val)
				(declare (type fixnum val))
				(parse-absolute (dims tensor) val)))
			 (etypecase dim
			   (list (map 'list #'helper dim))
			   (fixnum (helper dim))))))
  (declare (type AbstractTensor tensor)
	   (type (or list fixnum) dim))

  (assert (every #'null (tensor-broadcasted-axis tensor))
	  ()
	  "lazy-reduce: cannot reduce a broadcasted tensors.")
  
  (labels ((dim-eq (val)
	     (declare (type fixnum val))
	     (etypecase dim
	       (fixnum (= val dim))
	       (list   (member val dim :test #'=)))))
    (let* ((accumlate-to
	     (make-tensor
	      (loop for nth upfrom 0
		    for val in (tensor-shape tensor)
		    if (dim-eq nth)
		      collect 1
		    else
		      collect val)
	      (tensor-dtype tensor)
	      :layout (tensor-layout tensor)
	      :order  (tensor-order  tensor)))
	   (reduced-shape (loop for nth upfrom 0
				for val in (tensor-shape tensor)
				unless (dim-eq nth)
				  collect val)))
      (with-broadcasted-arrays (accumlate-to tensor)
	(let ((result
		(call
		 (make-op op :name 'lazy-reduce :next-outputs #'element-wise)
		 accumlate-to
		 tensor)))
	  (lazy-reshape result (or reduced-shape `(1))))))))

