
(cl:in-package :cl-user)

(defpackage :abstractnode.lisp-backend
  (:use :cl :AbstractNode.graph :AbstractNode.compiler))

(in-package :abstractnode.lisp-backend)

(defun merge-with (list key)
  (butlast
   (loop for l in list
	 append
	 `(,(format nil "~a" l) ,key))))

(defmethod compile-requirements ((backend-indicator (eql :lisp)))
  (format nil ";; Automatically Generated By AbstractNode.lisp~%DO NOT MODIFY THIS!~%"))

(defmethod compile-aref ((backend-indicator (eql :lisp)) tensor index)
  (format nil "(aref ~a ~a)"
	  (tensor-memory-id tensor)
	  index))

(defmethod compile-symbol ((backend-indicator (eql :lisp)) symbol)
  (format nil "~a" symbol))

(defmethod compile-dtype ((backend-indicator (eql :lisp)) dtype pointer-p)
  (symbol-macrolet ((dtype-helper 
		      (ecase dtype
			(:uint32
			 "(unsigned-byte 32)")
			(:double
			 "double-float")
			(:float
			 "single-float"))))
    (if pointer-p
	(format nil "(simple-array ~a (*))" dtype-helper)
	dtype-helper)))

(defmethod compile-iteration ((backend-indicator (eql :lisp))
			      index
			      from
			      to
			      by
			      body)
  (format nil "(loop for ~a of-type (unsigned-byte 64)
    upfrom ~a
    below ~a
    by ~a do~% ~a)" index from to by body))

(defmethod compile-function ((backend-indicator (eql :lisp))
			     name
			     vars
			     dynamic-shapes
			     body)
  (when (null body) (return-from compile-function ""))
  (with-output-to-string (out)
    (format out "(defun ~a (~a)"
	    name
	    (apply
	     #'concatenate
	     'string
	     (merge-with
	      `(,@(map 'list #'tensor-memory-id vars)
		,@dynamic-shapes)
	      " ")))
    (format out "~%(declare~%(optimize (speed 3))")    
    (loop for tensor in vars do
      (format out "~%(type ~a ~a)"
	      (compile-dtype backend-indicator
			     (tensor-dtype tensor)
			     (not (tensor-scalar-p tensor)))
	      (tensor-memory-id tensor)))
    (loop for shape  in dynamic-shapes do
      (format out "~%(type ~a ~a)"
	      :uint32
	      shape))
    (format out ")")
    (format out "~%~a)" body)))

(macrolet ((def (op lisp-op)
	     `(defmethod compile-instruction
		  ((backend-indicator (eql :lisp))
		   (op                (eql ,op))
		   &rest args)
		(format nil "(~a ~a)" ,lisp-op (apply #'concatenate 'string (merge-with args " "))))))
  (def :+ "+")
  (def :- "-")
  (def :* "*")
  (def :/ "/")
  (def := "setf"))

;; [TODO]
;;  - compile-newline
;;  - ./utils, scheduling, ir.lispをまともな実装にする
;;    - OpFusion, Loop Collapse, Polyhedral Compiler etc
;;  - Broadcasting, Reduce, Matmul
;;  - Slice, Unfold/folding

;; max/argmaxをどうやってvectorizeする？
;; reduce = broadcastで表現

(print
 (time
  (compile-with-backend
   :lisp
   (abop:lazy-add
    (abop:lazy-mul
     (make-tensor `(5 4 3) :float :input-p t)
     (make-tensor `(5 4 3) :float))
    (make-tensor `(5 4 3) :float)))))


