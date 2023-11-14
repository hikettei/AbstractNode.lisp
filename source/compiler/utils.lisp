
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
      ;;#+sbcl(declare (inline top-sort-helper))
      (dolist (toplevel toplevels)
	(top-sort-helper toplevel (tensor-detach-p toplevel)))
      (map 'list #'tensor-node (reverse top-sort)))))

