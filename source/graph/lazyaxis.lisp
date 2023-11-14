
(in-package :abstractnode.graph)

(defstruct (LazyAxis
	    (:constructor %make-lazyaxis (arguments form step)))
  (arguments arguments :type list)
  (form      (if (listp form)
		 (parse-lazy-exp form)
		 form))
  (constraints nil  :type list)
  (step        step :type function)
  (read-as     nil)
  (id          (gensym "axis")))

(defmethod print-object ((lazyaxis LazyAxis) stream)
  (format stream "[~a]" (lazyaxis-form lazyaxis)))

(defstruct (LazyIR
	    (:constructor make-lazyIR (type car cdr)))
  (type type :type (member :function :arithmetic :constant))
  (car  car)
  (cdr  cdr))

(defun ir->list (lazyir)
  (declare (type lazyir lazyir))
  (ecase (lazyir-type lazyir)
    (:constant     (lazyir-car lazyir))
    (:arithmetic   `(,(lazyir-car lazyir) ,@(map 'list #'ir->list (lazyir-cdr lazyir))))
    (:function     `(,(lazyir-car lazyir) ,@(map 'list #'ir->list (lazyir-cdr lazyir))))))

(defmethod print-object ((lazyir LazyIR) stream)
  (format stream "~a"
	  (ecase (lazyir-type lazyir)
	    (:constant (lazyir-car lazyir))
	    (:arithmetic
	     (with-output-to-string (out)
	       (format out "")
	       (dotimes (nth (length (lazyir-cdr lazyir)))
		 (format out "~a" (nth nth (lazyir-cdr lazyir)))
		 (unless (= nth (1- (length (lazyir-cdr lazyir))))
		   (format out "~a" (lazyir-car lazyir))))
	       (format out "")))
	    (:function
	     (with-output-to-string (out)
	       (format out "~(~a~)(" (lazyir-car lazyir))
	       (dotimes (nth (length (lazyir-cdr lazyir)))
		 (format out "~a" (nth nth (lazyir-cdr lazyir)))
		 (unless (= nth (1- (length (lazyir-cdr lazyir))))
		   (format out ", ")))
	       (format out ")"))))))

(defun interpret-lazy (lazyir)
  (declare (type LazyIR lazyir))
  (ecase (lazyir-type lazyir)
    (:constant
     (lazyir-car lazyir))
    (:arithmetic (apply (the symbol (lazyir-car lazyir)) (map 'list #'interpret-lazy (lazyir-cdr lazyir))))
    (:function   (apply (the symbol (lazyir-car lazyir)) (map 'list #'interpret-lazy (lazyir-cdr lazyir))))))

(defun parse-lazy-exp (exp)
  (trivia:ematch exp
    ((list* (or '+ '- '* '/) _)
     (parse-lazy-arithmetic (car exp) (cdr exp)))
    ((list* (or 'quote
		'#.(car ``nil))
	    _)
     (make-lazyir :constant  exp nil))
    ((list* (type symbol) _)     
     (multiple-value-bind (ident) (cl-environments:function-information (car exp))
       (when (or
	      (eql ident :special-form)
	      (eql ident :macro))
	 (warn "parse-lazy-exp: Don't include a macro form as LazySubscript: ~a, otherwise produces the wrong result.

Describe the transformation of shapes as simple as possible.
   LazySubscript is consisted of these elements:
        - Functions
        - Numbers
        - Symbols
   Other elements that effects on the result, should be written at outside of :where."
	       exp)))
     (parse-lazy-function (car exp) (cdr exp)))
    ((type number) (make-lazyir :constant exp nil))
    ((and (type symbol)
	  (not (type keyword)))
     (make-lazyir :constant exp nil))
    ((type LazyAxis) (make-lazyir :constant exp nil))
    (_  (make-lazyir :constant exp nil))))

(defun parse-lazy-arithmetic (car cdr)
  (let ((args (map 'list #'parse-lazy-exp cdr)))
    (if (every #'(lambda (ir)
		   (and (eql (lazyir-type ir) :constant)
			(typep (lazyir-car ir) 'fixnum)))
	       args)
	(make-lazyIR :constant
		     (apply car (map 'list #'lazyir-car args))
		     nil)
	;; A+0 -> A Mutation
	(let ((args
		(if (eql car '+)
		    (loop for arg in args
			  if (or
			      (not (eql (lazyir-type arg) :constant))
			      (not (eql (lazyir-car arg) 0)))
			    collect arg)
		    args)))
	  (if (and (eql car '+)
		   (= (length args) 1)
		   (numberp (car args)))
	      (make-lazyir :constant
			   (car args)			       
			   nil)
	      (make-lazyIR :arithmetic
			   car
			   args))))))

(defun parse-lazy-function (car cdr)
  (let ((args (map 'list #'parse-lazy-exp cdr)))
    (if (every #'(lambda (ir)
		   (and (eql (lazyir-type ir) :constant)
			(or (typep (lazyir-car ir) 'fixnum)
			    (typep (lazyir-car ir) 'boolean))))
	       args)
	(make-lazyIR :constant
		     (apply car (map 'list #'lazyir-car args))
		     nil)
	(make-lazyIR :function
		     car
		     (map 'list #'parse-lazy-exp cdr)))))

(defun ir->args (ir)
  "Collects a list of dynamic shapes used in the IR."
  (let ((result))
    (labels ((helper (ir-of)
	       (when (and
		      (eql (lazyir-type ir-of) :constant)
		      (symbolp (lazyir-car ir-of)))
		 (push (lazyir-car ir-of) result))
	       (dolist (var (reverse (lazyir-cdr ir-of)))
		 (helper var))))
      (helper ir)
      (delete-duplicates result))))

(defun make-lazyaxis (expression)
  "
## [function] make-lazyaxis

```lisp
(make-lazyaxis expression)
```

Creates a LazyAxis from given expression.

```lisp
(make-lazyaxis `(* A B))   ;; LazyAxis: (A B) -> A * B
(make-lazyaxis `(floor A)) ;; LazyAxis: A     -> floor(A)
(make-lazyaxis 1)          ;; LazyAxis: 1
(make-lazyaxis `(+ 1 1))   ;; LazyAxis: 2
```

No macro usings are allowed; functions and fixnum, list are available.
"
  (let* ((lazyir (parse-lazy-exp expression))
	 (args   (ir->args lazyir))
	 (form   (ir->list lazyir)))

    ;; If Parsed to constant -> Return as it is
    (when (and
	   (typep (lazyir-car lazyir) 'fixnum)
	   (eql (lazyir-type lazyir)  :constant))
      (return-from make-lazyaxis (lazyir-car lazyir)))

    (when (typep (lazyir-car lazyir) 'boolean)
      (return-from make-lazyaxis (lazyir-car lazyir)))
    
    (%make-lazyaxis
     args
     lazyir
     (if (listp form)
	 #'(lambda () (interpret-lazy lazyir))
	 #'(lambda () form)))))
