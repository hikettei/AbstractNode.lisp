
(in-package :abstractnode.graph)

(defun make-lazyaxis (expression)
  "
## [function] make-lazyaxis

```lisp
(make-lazyaxis expression)
```

expression := integer | symbol | form
form       := (op form1 form2 ... formN)

where op = a keyword indicating the operation.
"
  (labels ((confirm-helper (exp)
	     (typecase exp
	       (list
		(assert (keywordp (car exp))
			()
			"make-lazyaxis: car should be a keyword but got ~a" (car exp))
		(mapc #'confirm-helper (cdr exp))))))
    (confirm-helper expression))
  expression)

