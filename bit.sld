;; -*-  scheme -*-

;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis bit)
  (import (scheme base))
  (export logand logior logxor lognot ash)

  (begin
    ;; Common Lisp provides a full suite of bit operators and a bit vector
    ;; sequence datatype. Scheme, in its minimalism, provided neither
    ;; until R6RS required a minimal suite of bit operators, but still no
    ;; bit vectors. Our suite is small but useful. Note that all the bit
    ;; functions are grossly inefficient; you should use whatever your
    ;; Scheme implementation provides instead of relying on the functions
    ;; given below.

    ;; Bit-wise operators consider numbers as a sequence of binary bits
    ;; and operate on them using the logical operations and, inclusive-or,
    ;; exclusive-or, and not; they are implemented using basic arithmetic.
    ;; An arithmetic shift multiplies (positive shift) or divides
    ;; (negative shift) by powers of two:

    (define (logand a b)
      (if (or (zero? a) (zero? b)) 0
        (+ (* (logand (floor (/ a 2)) (floor (/ b 2))) 2)
          (if (or (even? a) (even? b)) 0 1))))

    (define (logior x y)
      (cond ((= x y) x)
        ((zero? x) y)
        ((zero? y) x)
        (else
          (+ (* (logior (quotient x 2) (quotient y 2)) 2)
            (if (and (even? x) (even? y)) 0 1)))))

    (define (logxor a b)
      (cond ((zero? a) b)
        ((zero? b) a)
        (else
          (+ (* (logxor (floor (/ a 2)) (floor (/ b 2))) 2)
            (if (even? a)
              (if (even? b) 0 1)
              (if (even? b) 1 0))))))

    (define (lognot a) (- -1 a))

    (define (ash int cnt)
      (if (negative? cnt)
        (let ((n (expt 2 (- cnt))))
          (if (negative? int)
            (+ -1 (quotient (+ 1 int) n))
            (quotient int n)))
        (* (expt 2 cnt) int)))

    ))
