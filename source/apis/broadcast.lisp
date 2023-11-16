
(in-package :abstractnode.apis)

(defun solve-broadcasts (&rest arrays)
  (labels ((the-tallest (arrays)
	     (loop for arr in arrays
		   unless (tensor-scalar-p arr)
		     maximize (length (tensor-shape arr))))
	   (rank-up-helper (arr
			    follow-to
			    &aux
			      (diff
			       (when follow-to
				 (- follow-to
				    (length (tensor-shape arr))))))
	     (or
	      (when (and follow-to
			 (not (tensor-scalar-p arr)))
		(lazy-reshape
		 arr
		 `(,@(make-list diff :initial-element 1)
		   ,@(tensor-shape arr))))
	      arr))
	   (broadcast-to (arrays)
	     #'(lambda (axis)
		 (let ((result
			 (find
			  1
			  arrays
			  :key
			  #'(lambda (x)
			      (shape-exp
			       (nth axis (tensor-shape x))))
			  :test-not #'eql)))
		   (if result
		       (nth axis (tensor-shape result))
		       (make-shape 1)))))
	   (broadcast-helper (arrays)
	     (let* ((tallest (the-tallest arrays))
		    (broadcast-to
		      (and
		       tallest
		       (map 'list (broadcast-to arrays) (range-list 0 tallest)))))
	       (loop for arr in arrays
		     if (or (tensor-scalar-p arr)
			    (equal (tensor-shape arr) broadcast-to))
		       collect arr
		     else
		       collect
		       (lazy-broadcast
			arr
			(loop for bc    in broadcast-to
			      for shape in (tensor-shape arr)
			      if (eql (shape-exp bc) (shape-exp shape))
				collect nil
			      else
				collect bc))))))
    
    (broadcast-helper
     (loop for arr in arrays
	   collect
	   (rank-up-helper arr (the-tallest arrays))))))

(defmacro with-broadcasted-arrays (arrays &body body)
  (if (symbolp arrays)
      `(let ((,arrays (apply #'solve-broadcasts ,arrays)))
	 ,@body)
      (let ((placeholder (gensym)))
	`(let* ((,placeholder (apply #'solve-broadcasts (list ,@arrays)))
		,@(loop for arr in arrays
			for nth upfrom 0
			collect
			`(,arr (nth ,nth ,placeholder))))
	   ,@body))))

