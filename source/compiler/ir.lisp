
(in-package :abstractnode.compiler)

;; Topological Sorting
;; Memory-Locality Optimizing
;; Thread-Safe In-Place Mutation


(defun compile-ir (backend-indicator ir)
  ir
  )

(defun compile-with-backend (backend &rest tensors)
  (let* ((sorted-ir  (topological-sort tensors)))
    (with-output-to-string (out)
      ;; First, collecting all dynamic shaped used in the node
      ;; Secondly, scheduling and gathering to find out functions needs to be compiled
      ;; Reduce the amount of compiled code by caching by LUT
      ;; (Optional) Autograd      
      (compile-requirements backend)
      (print sorted-ir)
      )))

(defun compute-with-backend (backend &rest tensors)

  )
