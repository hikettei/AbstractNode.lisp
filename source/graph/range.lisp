
(in-package :abstractnode.graph)

(defun lazy-max (a b)
  "Given the condition that dynamic shape is given as positive fixnum, this function lazily computes maximum value of a and b."
  (if (eql a 0)
      b
      (if (eql b 0)
	  a
	  `(:max ,a ,b))))

(defun lazy-min (a b)
  "Given the condition that dynamic shape is given as positive fixnum, this function lazily computes minimum value of a and b."
  (if (eql a 0)
      a
      (if (eql b 0)
	  b
	  `(:min ,a ,b))))

(defstruct (Range
	    (:conc-name range-)
	    (:constructor make-range (from to &optional (step 1))))
  (from from :type (or fixnum list symbol))
  (to   to   :type (or fixnum list symbol))
  (step step :type fixnum))

(defun lazy-range-size (range)
  (declare (type range range))
  (make-lazyaxis
   `(:floor
     (:- ,(range-to range) ,(range-from range))
     ,(range-step range))))

(defmethod print-object ((obj Range) stream)
  (format stream
	  (if (numberp (lazy-range-size obj))
	      "<Range For ~a, [~a, ~a), step=~a>"
	      "<Range~%    For [~a]~%      from ~a~%      to ~a~%      stepby ~a>")	      
	  (lazy-range-size obj)
	  (range-from obj)
	  (range-to   obj)
	  (range-step obj)))

(defun range-size (range)
  "Computes the number of iterations of range as simple as possible:
Basically can be computed in this formula:
    floor(ABS(from - to) // STEP)"
  (declare (type range range))
  (if (= (range-step range) 1)
      (if (and (numberp  (range-from range))
	       (= 0      (range-from range)))
	  (make-lazyaxis (range-to range))
	  (make-lazyaxis
	   `(:- ,(range-to range)
		,(range-from range))))
      (make-lazyaxis
       `(:floor
	 (:- ,(range-to range)
	     ,(range-from   range))
	 ,(range-step range)))))

(defun range (from &optional (to `(1+ ,from)) (step 1))
  "
## [function] range

```lisp
(range from &optional (to `(1+ ,from)) (step 1))
```

Creates a range: `[from, to) where step=step`. This structure is dedicated to a do-range macro which generates an iteration following rules:

- starting from (min from, to)
- ends with (max from, to)
- if step < 0, the order is reversed.
- This macro asserts that `var` is in the range of [from, to).
"
  (assert (or
	   (not (numberp step))
	   (not (= step 0)))
	  ()
	  "range: do not create a range whose step is 0 otherwise loop continues forever.")

  (assert (integerp step)
	  ()
	  "range: Assertion failed because step should be given as an integer butgot: ~a" step)
  ;; [TODO] Inserting LazyAssertion which is: from > to
  (when (or (symbolp from)
	    (symbolp to))
    nil)
  
  (make-range from to step))

(defun .range (range2 &optional (range1 nil))
  "
## [function] .range

```lisp
(range+ range2 &optional (range1 nil))
```

Interprets the value of range2 from a viewpoint of range1.

`Original Tensor -> range2 -> [View] -> range1`

```
Applying a further slicing:
    (Range 2 10 2) ;; range1
 +) (Range 0 4  2) ;; range2
 ------------------
    (Range 2 6 2)

is defined as:
 A = min(range1.from, range1.to)
 B = min(range2.from, range2.to)
 C = max(range2.from, range2.to)

new_range_from  = A + B
new_range_to    = A + C
new_range_step  = lcm(range1.step, range2.step)

i.e.:
{a_2x+b_2}∪{a_1x+b_1} 
```
"
  (declare (type Range range2)
	   (type (or null Range) range1))

  (when (null range1)
    ;; NIL(RANGE1(...))
    (return-from .range range2))

  ;; range1 = base
  ;; range2 = new

  ;; Dynamic Shape is asserted to be a positive number
  (let* ((upfrom1 (range-from range1))
	 (below1  (range-to   range1))

	 (upfrom2 (range-from range2))
	 (below2  (range-to   range2))

	 (step1   (range-step range1))
	 (step2   (range-step range2))

	 (offset (if (> step1 0)
		     (lazy-min upfrom1 below1)
		     (lazy-max upfrom1 below1)))

	 (from  `(:+ ,offset (:* ,(signum step1) ,upfrom2)))
	 (to    `(:+ ,offset (:* ,(signum step1) ,below2)))
	 
	 (from1 (lazy-min from to))
	 (to1   (lazy-max from to))
	 (step  (* (signum step2) (lcm step1 step2))))
    ;; [TODO]
    ;; - Adding an assertion
    ;; - Adding a lazy assertion if one of from/to/step is a symbol.
    (let ((result (range from1 to1 step)))
      result)))

