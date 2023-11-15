
(in-package :abstractnode.compiler)

(declaim (ftype (function (list) list) topological-sort))
#+sbcl(setf sb-ext:*inline-expansion-limit* 30)
(defun topological-sort (toplevels)
  (let ((seen     (make-hash-table :test #'eql))
	(top-sort nil))
    (labels ((top-sort-helper (v is-leaf-p &aux (node (tensor-node v)))
	       (if (or
		    (null node)
		    (gethash (abstractnode-id node) seen)
		    is-leaf-p)
		   nil
		   (progn
		     (setf (gethash (abstractnode-id node) seen) t)
		     (dolist (prev (tensor-variables v))
		       (top-sort-helper prev (tensor-detach-p v)))
		     (push v top-sort)))))
      #+sbcl(declare (inline top-sort-helper))
      (dolist (toplevel toplevels)
	(top-sort-helper toplevel (tensor-detach-p toplevel)))
      (map 'list #'tensor-node (reverse top-sort)))))

(defun blueprint2fname (backend-indicator blueprint)
  (compile-symbol
   backend-indicator
   (format nil "~a~a"
	   (blueprint-name blueprint)
	   (apply
	    #'concatenate
	    'string
	    (butlast
	     (loop for iter in (blueprint-iterators blueprint)
		   append
		   `(,(format nil "~a" (iterator-rank iter))
		     "_"
		     ,(format nil "~a" (iterator-from iter))
		     "_"
		     ,(format nil "~a" (iterator-to   iter))
		     "_"
		     ,(format nil "~a" (iterator-by iter))
		     "_")))))))

(defun iter-n (backend-indicator n)
  (compile-symbol backend-indicator (format nil "grid~a" n)))

(defun bp-variables (insts)
  (let ((vars))
    (dolist (ir insts)
      (dolist (var `(,@(abstractnode-in-args ir)))
	(push var vars)))
    (remove-duplicates vars :key #'tensor-memory-id)))

