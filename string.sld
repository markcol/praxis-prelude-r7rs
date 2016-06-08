;; -*-  scheme -*-
;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis string)
  (import (scheme base)
	  (scheme char))
  (export string-index string-downcase string-upcase string-split
          string-join string-find)

  (begin
    ;; (String-index c str) returns the zero-based index of the first
    ;; occurrence of c in str, or #f if c does not appear in str:

    (define (string-index c str)
      (let loop ((ss (string->list str)) (k 0))
        (cond ((null? ss) #f)
	      ((char=? (car ss) c) k)
	      (else (loop (cdr ss) (+ k 1))))))

    ;; String-split takes a separator character and a string and
    ;; returns a list of sub-strings bounded by the separator
    ;; character. String-join is the inverse.
    (define (string-split sep str)
      (define (f cs xs) (cons (list->string (reverse cs)) xs))
      (let loop ((ss (string->list str)) (cs '()) (xs '()))
        (cond ((null? ss) (reverse (if (null? cs) xs (f cs xs))))
	      ((char=? (car ss) sep) (loop (cdr ss) '() (f cs xs)))
	      (else (loop (cdr ss) (cons (car ss) cs) xs)))))

    (define (string-join sep ss)
      (define (f s ss)
        (string-append s (string sep) ss))
      (define (join ss)
        (if (null? (cdr ss)) (car ss)
	    (f (car ss) (join (cdr ss)))))
      (if (null? ss) "" (join ss)))

    ;; String-find returns the starting position of a pattern in a
    ;; string, or #f if the string does not contain the pattern; it
    ;; uses the Knuth-Morris-Pratt string search algorithm:

    (define (string-find pat str . s)
      (let* ((plen (string-length pat))
	     (slen (string-length str))
	     (skip (make-vector plen 0)))
        (let loop ((i 1) (j 0))
          (cond ((= i plen))
		((char=? (string-ref pat i) (string-ref pat j))
		 (vector-set! skip i (+ j 1))
		 (loop (+ i 1) (+ j 1)))
		((< 0 j) (loop i (vector-ref skip (- j 1))))
		(else (vector-set! skip i 0)
		      (loop (+ i 1) j))))
        (let loop ((p 0) (s (if (null? s) 0 (car s))))
          (cond ((= s slen) #f)
		((char=? (string-ref pat p) (string-ref str s))
		 (if (= p (- plen 1))
		     (- s plen -1)
		     (loop (+ p 1) (+ s 1))))
		((< 0 p) (loop (vector-ref skip (- p 1)) s))
		(else (loop p (+ s 1)))))))

    ))
