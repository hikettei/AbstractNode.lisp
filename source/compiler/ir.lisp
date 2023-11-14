
(in-package :abstractnode.compiler)

;; Topological Sorting
;; Memory-Locality Optimizing
;; Thread-Safe In-Place Mutation

(defun compile-instructions (backend-indicator bp nrank)
  ""
  )

(defun compile-blueprint (backend-indicator name bp &aux (nrank (length (blueprint-iterators bp))))
  (declare (type blueprint bp))
  (symbol-macrolet ((iterators
		      (blueprint-iterators bp)))
    (labels ((helper (rank)
	       (if (= rank (1- nrank))
		   (compile-instructions
		    backend-indicator
		    bp
		    nrank)
		   (compile-iteration-helper
		    backend-indicator
		    (iter-n rank)
		    (iterator-from (nth rank iterators))
		    (iterator-to   (nth rank iterators))
		    (iterator-by   (nth rank iterators))
		    (helper (1+ rank))))))
      (compile-function
       backend-indicator
       name
       (bp-variables (blueprint-instructions bp))
       (blueprint-dynamic-shapes bp)
       (helper 0)))))

(defun overwrite-memory-id! (sorted-ir from-id to-id)
  (declare (type list sorted-ir)
	   (type symbol from-id to-id))
  (dolist (ir sorted-ir)
    (dolist (arg `(,@(abstractnode-in-args  ir)
		   ,@(abstractnode-out-args ir)))
      (when (eql (tensor-memory-id arg) from-id)
	(setf (tensor-memory-id arg) to-id)))))

(defun shared-buffer-schedule! (sorted-ir)
  sorted-ir)

(defun apply-in-place-mutation! (sorted-ir
				 &aux
				   (ref-count (make-ref-count-table sorted-ir)))
  (macrolet ((id-of (tensor)
	       `(gethash (tensor-memory-id ,tensor) ref-count)))
    (loop for nth fixnum upfrom 0
	  for ir  in sorted-ir
	  if (abstractnode-in-place-mutation-p ir) do
	    ;; Move: OUT-TO <- OP(OUT-TO, ARG)
	    (let* ((out-to    (car (abstractnode-out-args ir)))
		   (copy-from (find-if
			       #'(lambda (arg)
				   (not (eql (tensor-memory-id arg) (tensor-memory-id out-to))))
			       (abstractnode-in-args ir))))
	      (assert (and out-to copy-from)
		      ()
		      "apply-in-place-mutation!: Assertion Failed because the tensor used in out-to wasn't appeared in the arguments.
IR:
~a"
		      ir)
	      (when (= 0 (id-of copy-from))
		(overwrite-memory-id! (nthcdr nth sorted-ir) (tensor-memory-id out-to) (tensor-memory-id copy-from))))
	  else do
	    (dolist (arg (abstractnode-in-args ir))
	      (when (id-of arg)
		;; Consuming the reference count
		(decf (id-of arg)))))
    (loop for IR in sorted-ir
	  if (or (not (abstractnode-in-place-mutation-p IR))
		 ;; [FixME] optimize me ;(
		 (let* ((out-to    (car (abstractnode-out-args ir)))
			(copy-from (find-if
				    #'(lambda (arg)
					(not (eql (tensor-id arg) (tensor-id out-to))))
				    (abstractnode-in-args ir))))
		   (not (eql (tensor-memory-id out-to) (tensor-memory-id copy-from)))))
	    collect IR)))		       

(defun make-ref-count-table (sorted-ir &key (sort-by #'tensor-memory-id))
  (declare (type list sorted-ir)
	   (optimize (speed 3)))
  (let ((reference-count (make-hash-table)))
    (macrolet ((id-of (tensor)
		 `(gethash (funcall (the function sort-by) ,tensor) reference-count)))
      ;; First, creates a table recording reference count of all variables
      ;; Variables has a two state:
      ;;  TopLevel : tensors whose node=nil    -> refcount is set to -1 which can't be deleted
      ;;  Chain    : those who created by call -> refcount is set to 0
      (dolist (ir sorted-ir)
	(dolist (var (abstractnode-in-args ir))
	  (if (null (id-of var))
	      ;; First Time Creation
	      (if (tensor-input-p var)
		  (setf (id-of var) -1)
		  (setf (id-of var) 0))
	      (if (tensor-input-p var)
		  nil
		  (incf (the fixnum (id-of var))))))))
    reference-count))

(defun compile-with-backend (backend &rest tensors)
  (let* ((sorted-ir  (topological-sort tensors))
	 (sorted-ir  (delete-if #'abstractnode-system-ir-p sorted-ir))
	 (sorted-ir  (apply-in-place-mutation! sorted-ir))
	 (sorted-ir  (shared-buffer-schedule!  sorted-ir))
	 (id->cname (make-hash-table :test #'equal))
	 (bp-table  (make-hash-table :test #'equal)))
    (multiple-value-bind (blueprints dynamic-shapes) (make-scheduling sorted-ir)
      (loop for bp in blueprints do
	(setf (gethash (blueprint-id bp) id->cname) (gensym "OP")
	      (gethash (blueprint-id bp) bp-table)  bp))	    
      (with-output-to-string (out)
	;; First, collecting all dynamic shaped used in the node
	;; Secondly, scheduling and gathering to find out how many functions (and blueprint) needs to be compiled
	;; Reduce the amount of compiled code by caching by LUT
	;; (Optional) Autograd
	(format out "~a" (compile-requirements backend))

	;; (compile-function-declarations )

	(loop for bp being the hash-values of bp-table do
	  (format out "~a~%~%"
		  (compile-blueprint
		   backend
		   (gethash (blueprint-id bp) id->cname)
		   bp)))
	))))

(defun compute-with-backend (backend &rest tensors)

  )
