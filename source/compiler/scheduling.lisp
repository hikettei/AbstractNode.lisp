
(in-package :abstractnode.compiler)

;; [TODO] Much clever way to implement the all code below:

(defun dynamic-shape-symbols (sorted-ir)
  (let ((dynamic-shapes))    
    (dolist (IR sorted-ir)
      (dolist (var (abstractnode-in-args IR))
	(dolist (shape (tensor-shape var))
	  (let ((subject
		  (if (tensor-scalar-p var)
		      (or (when (shape-p (tensor-storage var)) (shape-exp (tensor-storage var)))
			  (tensor-storage var))
		      (shape-exp shape))))
	    ;; A Symbol but nil
	    (when (and subject (symbolp subject))
	      (push subject dynamic-shapes))))))
    (delete-duplicates dynamic-shapes)))

(defun optimize-need-compiled-p (backend-indicator blueprints)
  (let ((seen (make-hash-table :test #'equal)))
    (loop for bp in blueprints do
      (setf (blueprint-id bp) (blueprint2fname backend-indicator bp)))
    (loop for bp in blueprints do
      (if (null (gethash (blueprint-id bp) seen))
	  (setf (gethash (blueprint-id bp) seen) t)
	  (setf (blueprint-need-compile-p bp) nil)))))

(defstruct Iterator rank from to by)

(defun make-iterations (ir)
  (let ((rep (car (abstractnode-in-args ir))))
    (loop for rank upfrom 0
	  for shape in (tensor-shape rep)
	  collect
	  (make-iterator
	   :rank rank
	   :from 0
	   :to   (nth rank (tensor-shape rep))
	   :by   1))))

(defstruct Blueprint
  (dynamic-shapes)
  (id)
  (name)
  (need-compile-p t)
  (iterators)
  (instructions))

;; [FixME] Cacheの作成方法, Stride/OffsetsをConsidering
;; [TODO] OpFusion, Multi-threading scheduling
(defun make-scheduling (backend-indicator sorted-ir)
  (let ((dynamic-shapes (dynamic-shape-symbols sorted-ir))
	(blueprints))

    (dolist (ir sorted-ir)
      (push (make-blueprint
	     :dynamic-shapes (dynamic-shape-symbols (list ir))
	     :name (abstractnode-name ir)
	     :iterators    (make-iterations ir)
	     :instructions (list ir))
	    blueprints))

    (optimize-need-compiled-p backend-indicator blueprints)
    
    (values (reverse blueprints) dynamic-shapes)))

