;; -*-  scheme -*-
;; From http://programmingpraxis.com/contents/standard-prelude/

;; Lists are ubiquitous in Scheme, so it is useful to have available a
;; collection of utility functions that operate on lists.

(define-library (praxis list)
  (import (scheme base)
	  (scheme cxr))
  (export all? any? cons* cross drop drop-while filter flatten
	  fold-left fold-right iterate mappend maximum-by range
	  remove split split-while take take-while zip
	  ;; provided by 'scheme base':
	  make-list sum)

  (begin

    ;; Take returns a newly-allocated list containing the first n elements
    ;; of the list xs; if xs has less than n elements, return all of them.
    (define (take n xs)
      (let loop ((n n) (xs xs) (ys '()))
        (if (or (zero? n) (null? xs))
	    (reverse ys)
	    (loop (- n 1) (cdr xs)
		  (cons (car xs) ys)))))

    ;; Drop is the opposite of take, returning all elements of a list xs
    ;; except the first n.
    (define (drop n xs)
      (let loop ((n n) (xs xs))
        (if (or (zero? n) (null? xs)) xs
	    (loop (- n 1) (cdr xs)))))

    ;; Split combines take and drop:
    (define (split n xs)
      (let loop ((n n) (xs xs) (zs '()))
        (if (or (zero? n) (null? xs))
	    (values (reverse zs) xs)
	    (loop (- n 1) (cdr xs) (cons (car xs) zs)))))

    ;; Take-while, drop-while, and split-while are similar to take, drop
    ;; and split, but instead of counting elements, they operate on that
    ;; prefix of the elements x of xs for which (pred? x) is non-#f:
    (define (take-while pred? xs)
      (let loop ((xs xs) (ys '()))
        (if (or (null? xs) (not (pred? (car xs))))
	    (reverse ys)
	    (loop (cdr xs) (cons (car xs) ys)))))

    (define (drop-while pred? xs)
      (let loop ((xs xs))
        (if (or (null? xs) (not (pred? (car xs)))) xs
	    (loop (cdr xs)))))

    (define (split-while pred? xs)
      (let loop ((xs xs) (ys '()))
        (if (or (null? xs) (not (pred? (car xs))))
	    (values (reverse ys) xs)
	    (loop (cdr xs) (cons (car xs) ys)))))

    ;; Cons* is similar to list, but handles the last pair differently;
    ;; (list 1 2 3) forms the list (1 2 3 . ()), but (cons* 1 2 3) forms
    ;; the list (1 2 . 3):
    (define (cons* first . rest)
      (let loop ((curr first) (rest rest))
        (if (null? rest) curr
	    (cons curr (loop (car rest) (cdr rest))))))

    ;; Folds use a user-specified function to reduce a list of values to a
    ;; single value, and are one of the fundamental idioms of functional
    ;; programming.

    ;; Fold-left works left-to-right through the list xs,
    ;; applying the binary op function to base and the first element of
    ;; xs, then applying the binary op function to the result of the first
    ;; op function and the second element of xs, and so on, at each step
    ;; applying the binary op function to the result of the previous op
    ;; function and the current element of xs

    ;; fold-right works the same way, but right-to-left.

    (define (fold-left op base xs)
      (if (null? xs)
	  base
	  (fold-left op (op base (car xs)) (cdr xs))))

    (define (fold-right op base xs)
      (if (null? xs)
	  base
	  (op (car xs) (fold-right op base (cdr xs)))))

    ;; The (range [first] past [step]) function takes three arguments and
    ;; returns a list of numbers starting from first and ending before
    ;; past, incrementing each number by step. If step is omitted, it
    ;; defaults to 1 if first is less than past, and -1 otherwise; if
    ;; first is also omitted, it defaults to 0. Arguments may be of any
    ;; numeric type.

    (define (range . args)
      (case (length args)
        ((1) (range 0 (car args) (if (negative? (car args)) -1 1)))
        ((2) (range (car args) (cadr args) (if (< (car args) (cadr args)) 1 -1)))
        ((3) (let ((le? (if (negative? (caddr args)) >= <=)))
               (let loop ((x(car args)) (xs '()))
                 (if (le? (cadr args) x)
		     (reverse xs)
		     (loop (+ x (caddr args)) (cons x xs))))))
        (else (error 'range "unrecognized arguments"))))

    ;; Mappend is like map, but assembles its pieces with append rather
    ;; than cons. Iterate repeatedly evaluates a function f against the
    ;; base values bs, after each iteration shifting the base values left
    ;; by removing the first and appending the newly-calculated value at
    ;; the end, returning the first n results in a list:

    (define (mappend f . xss) (apply append (apply map f xss)))

    (define (iterate n f . bs)
      (let loop ((n n) (b (car bs)) (bs (cdr bs)) (xs '()))
        (if (zero? n) (reverse xs)
	    (let ((new-bs (append bs (list (apply f b bs)))))
	      (loop (- n 1) (car new-bs) (cdr new-bs) (cons b xs))))))

    ;; For instance, (define (fib n) (iterate n + 1 1)) defines a function
    ;; that calculates the first n fibonacci numbers

    ;; The (filter pred? xs) function returns a newly-allocated list that
    ;; contains only those elements x of xs for which (pred? x) is true.
    ;; The (remove x xs) function removes all occurrences of x from the
    ;; list xs, using equal? to make comparisons:

    (define (filter pred? xs)
      (let loop ((xs xs) (ys '()))
        (cond ((null? xs) (reverse ys))
	      ((pred? (car xs))
	       (loop (cdr xs) (cons (car xs) ys)))
	      (else (loop (cdr xs) ys)))))

    (define (remove x xs)
      (let loop ((xs xs) (zs '()))
        (cond ((null? xs) (reverse zs))
	      ((equal? (car xs) x) (loop (cdr xs) zs))
	      (else (loop (cdr xs) (cons (car xs) zs))))))

    ;; Flatten takes a tree represented as a list with sub-lists and
    ;; returns a list containing only the fringe elements of the tree. The
    ;; two trees (a (b c)) and ((a b) c) both cause flatten to return (a b
    ;; c):

    (define (flatten xs)
      (cond ((null? xs) xs)
	    ((pair? xs)
	     (append (flatten (car xs))
		     (flatten (cdr xs))))
	    (else (list xs))))

    ;; All? and any? apply a predicate to each member of a list. All?
    ;; returns #t if (pred? x) is non-#f for every x in the input list, or
    ;; #f otherwise. Any? returns #f if (pred? x) is #f for every x in the
    ;; input list, or #t otherwise. Both functions stop applying the
    ;; predicate to elements of the input as list as soon as possible.

    (define (all? pred? xs)
      (cond ((null? xs) #t)
	    ((pred? (car xs))
	     (all? pred? (cdr xs)))
	    (else #f)))

    (define (any? pred? xs)
      (cond ((null? xs) #f)
	    ((pred? (car xs)) #t)
	    (else (any? pred? (cdr xs)))))

    ;; Zip converts multiple lists into a list of lists:
    (define (zip . xss) (apply map list xss))

    ;; Cross produces the cross product of one or more lists. The
    ;; implementation is a functional pearl due to Christoper Strachey.
    (define (cross . xss)
      (define (f xs yss)
        (define (g x zss)
          (define (h ys uss)
            (cons (cons x ys) uss))
          (fold-right h zss yss))
        (fold-right g '() xs))
      (fold-right f (list '()) xss))

    ;; Sum calculates the sum of the input list.
    (define (sum xs) (apply + xs))

    ;; Maximum-by is like the built-in function max, except that it takes
    ;; a lt? comparison function as its first argument before the items to
    ;; be compared:
    (define (maximum-by lt? . xs)
      (let loop ((xs (cdr xs)) (current-max (car xs)))
        (cond ((null? xs) current-max)
	      ((lt? current-max (car xs))
	       (loop (cdr xs) (car xs)))
	      (else (loop (cdr xs) current-max)))))

    ))
