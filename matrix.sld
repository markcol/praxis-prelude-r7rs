;; -*-  scheme -*-
;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis matrix)
  (import (scheme base))
  (export make-matrix matrix-rows matrix-cols matrix-ref matrix-set! for)

  (begin
    ;; Scheme provides one-dimensional arrays, which it calls vectors, but
    ;; no two-dimensional arrays. Kent Dybvig provides a matrix data
    ;; structure defined as a vector of vectors in The Scheme Programming
    ;; Language:

    (define (make-matrix rows columns . value)
      (do ((m (make-vector rows)) (i 0 (+ i 1)))
	  ((= i rows) m)
        (if (null? value)
	    (vector-set! m i (make-vector columns))
	    (vector-set! m i (make-vector columns (car value))))))

    (define (matrix-rows x) (vector-length x))

    (define (matrix-cols x) (vector-length (vector-ref x 0)))

    (define (matrix-ref m i j) (vector-ref (vector-ref m i) j))

    (define (matrix-set! m i j x) (vector-set! (vector-ref m i) j x))

    ;; The for macro is convenient for iterating over the rows and columns
    ;; of a matrix. The syntax (for (var [first] past [step]) body ...)
    ;; binds var to first, then iterates var by step until it reaches
    ;; past, which is not bound; the body statements are executed only for
    ;; their side-effects. Step defaults to 1 if first is less than past
    ;; and -1 otherwise; if first is also not given, it defaults to 0:

    (define-syntax for
      (syntax-rules ()
        ((for (var first past step) body ...)
	 (let ((ge? (if (< first past) >= <=)))
	   (do ((var first (+ var step)))
	       ((ge? var past))
	     body ...)))
        ((for (var first past) body ...)
	 (let* ((f first) (p past) (s (if (< first past) 1 -1)))
	   (for (var f p s) body ...)))
        ((for (var past) body ...)
	 (let* ((p past)) (for (var 0 p) body ...)))))

    ))
