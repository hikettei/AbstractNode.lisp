
(in-package :abstractnode.compiler)

(defun compile-lazy-index (backend-indicator shape)
  "
## [function] compile-lazy-index

```lisp
(compile-lazy-index backend-indicator shape)
```

Translates S-expression into backend-indicator

```lisp
S-expression := integer | symbol | form
form := (op form1 form2 ...)
where op = keyword
```
Inputs:
- `Shape[abstractnode.graph:shape]`
"
  (declare (type Shape shape))
  (etypecase (shape-exp shape)
    (integer
     (shape-exp shape))
    (symbol
     (compile-symbol backend-indicator (shape-exp shape)))
    (string
     (shape-exp shape))
    (list
     (let ((op   (car (shape-exp shape)))
	   (args
	     (map
	      'list
	      #'(lambda (x)
		  (compile-lazy-index
		   backend-indicator
		   (make-shape x)))
	      (cdr (shape-exp shape)))))
       (apply
	#'compile-instruction
	backend-indicator
	op
	args)))))

