;; -*-  scheme -*-
;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis comprehension)
  (import (scheme base))
  (export fold-of list-of sum-of nth-of)

  (begin
    ;; List comprehensions join the higher-order idioms map, filter and
    ;; fold into a highly useful form of syntactic sugar for performing
    ;; looping computations involving lists. The Standard Prelude provides
    ;; three types of list comprehension:

    ;;(list-of expr clause ...) produces a possibly-null list of objects
    ;; of the type returned by expr

    ;; (sum-of expr clause ...) computes the sum of the elements computed
    ;; by expr

    ;; (fold-of op base expr clause ...) is like sum-of, but with a
    ;; user-defined operator and base in place of the + and 0 of sum-of

    ;; Clauses may be of four types:

    ;; (var range [first] past [step]) — Bind var to first, first + step,
    ;; ..., until reaching past, which is not included in the output. If
    ;; first is not given it defaults to 0. If step is not given, it
    ;; defaults to 1 if (< first past) and -1 otherwise. First, past and
    ;; step may be of any numeric type; if any of first, past or step are
    ;; inexact, the length of the output list may differ from (ceiling (-
    ;; (/ (- past first) step) 1).

    ;; (var in list-expr) — Loop over the elements of list-expr, in order
    ;; from the start of the list, binding each element of the list in
    ;; turn to var.

    ;; (var is expr) — Bind var to the value obtained by evaluating expr.

    ;; (pred? expr) — Include in the output list only those elements x for
    ;; which (pred? x) is non-#f.

    ;; The scope of variables bound in the list comprehension is the
    ;; clauses to the right of the binding clause (but not the binding
    ;; clause itself) plus the result expression. When two or more
    ;; generators are present, the loops are processed as if they are
    ;; nested from left to right; that is, the rightmost generator varies
    ;; fastest. As a degenerate case, if no generators are present, the
    ;; result of a list comprehension is a list containing the result
    ;; expression; thus, (list-of 1) produces a list containing only the
    ;; element 1.

    ;; List comprehensions are expanded by a macro that calls itself
    ;; recursively, one level of recursion for each clause plus a final
    ;; level of recursion for the base case. The complete implementation,
    ;; which is based on the set constructor in Kent Dybvig’s book The
    ;; Scheme Programming Language, is given below:

    (define-syntax fold-of
      (syntax-rules (range in is)
        ((_ "z" f b e) (set! b (f b e)))
        ((_ "z" f b e (v range fst pst stp) c ...)
	 (let* ((x fst) (p pst) (s stp)
		(le? (if (positive? s) <= >=)))
	   (do ((v x (+ v s))) ((le? p v) b)
	     (fold-of "z" f b e c ...))))
        ((_ "z" f b e (v range fst pst) c ...)
	 (let* ((x fst) (p pst) (s (if (< x p) 1 -1)))
	   (fold-of "z" f b e (v range x p s) c ...)))
        ((_ "z" f b e (v range pst) c ...)
	 (fold-of "z" f b e (v range 0 pst) c ...))
        ((_ "z" f b e (x in xs) c ...)
	 (do ((t xs (cdr t))) ((null? t) b)
	   (let ((x (car t)))
	     (fold-of "z" f b e c ...))))
        ((_ "z" f b e (x is y) c ...)
	 (let ((x y)) (fold-of "z" f b e c ...)))
        ((_ "z" f b e p? c ...)
	 (if p? (fold-of "z" f b e c ...)))
        ((_ f i e c ...)
	 (let ((b i)) (fold-of "z" f b e c ...)))))

    (define-syntax list-of
      (syntax-rules ()
	((_ arg ...) (reverse (fold-of
			       (lambda (d a) (cons a d)) '() arg ...)))))

    (define-syntax sum-of
      (syntax-rules ()
	((_ arg ...) (fold-of + 0 arg ...))))

    ;; The list comprehensions given here extend the usual notion of list
    ;; comprehensions with the fold-of comprehension. Another extension is
    ;; nth-of, which expands a list comprehension and returns the nth
    ;; item:
    (define-syntax nth-of
      (syntax-rules ()
        ((_ n expr clause ...)
	 (let ((nth n))
	   (call-with-current-continuation
	    (lambda (return)
	      (fold-of
	       (lambda (fst snd)
		 (if (zero? nth) (return snd)
		     (begin (set! nth (- nth 1)) snd)))
	       #f expr clause ...)))))))

    ))
