;; -*-  scheme -*-

;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis bit-vector)
  (import (scheme base)
          (praxis bit))
  (export make-bitvector bitvector-ref bitvector-set!
          bitvector-reset! bitvector-count bitvector-length)
  (begin
    ;; Bit vectors are implemented using arrays of characters. They are
    ;; represented as a pair with a vector of eight-bit characters in the
    ;; car and the length of the bit vector in the cdr. Make-bitvector
    ;; creates a bit vector of the requested length with all bits zero
    ;; unless the optional argument val is one; note the logic to ensure
    ;; any “slop” bits at the end of the bit vector are set to zero, which
    ;; is useful in the bitvector-count function:

    (define (make-bitvector len . val)
      (let ((v (make-vector
                 (ceiling (/ len 8))
                 (if (and (pair? val) (= (car val) 1)) 255 0))))
        (if (and (pair? val) (= (car val) 1) (not (zero? (modulo len 8))))
          (do ((i 8 (- i 1))) ((= i (modulo len 8)))
            (vector-set! v (floor (/ len 8))
              (logand (vector-ref v (floor (/ len 8))) (lognot (ash 1 (- i 1)))))))
        (cons v len)))

    ;; Bitvector-ref returns the value of bv[idx], either zero or one:

    (define (bitvector-ref bv idx)
      (if (< -1 idx (cdr bv))
        (let ((index (quotient idx 8)) (offset (modulo idx 8)))
          (if (odd? (ash (vector-ref (car bv) index) (- offset))) 1 0))
        (error 'bitvector-ref "out of range")))

    ;; Bitvector-set! sets the requested bit to one. Bitvector-reset! sets
    ;; the requested bit to zero:

    (define (bitvector-set! bv idx)
      (if (< -1 idx (cdr bv))
        (let ((index (quotient idx 8)) (offset (modulo idx 8)))
          (vector-set! (car bv) index
            (logior (vector-ref (car bv) index) (ash 1 offset))))
        (error 'bitvector-set! "out of range")))

    (define (bitvector-reset! bv idx)
      (if (< -1 idx (cdr bv))
        (let ((index (quotient idx 8)) (offset (modulo idx 8)))
          (vector-set! (car bv) index
            (logand (vector-ref (car bv) index) (lognot (ash 1 offset)))))
        (error 'bitvector-reset! "out of range")))

    ;; Bitvector-count returns the number of one-bits in the input bit
    ;; vector. The counts per byte are pre-calculated in the counts
    ;; vector. Note that the last byte is not special, because
    ;; make-bitvector was careful to ensure that any “slop” bits are zero:

    (define (bitvector-count bv)
      (let* ((counts #(
                        0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4 1 2 2 3 2 3 3 4 2 3 3 4 3 4 4 5
                        1 2 2 3 2 3 3 4 2 3 3 4 3 4 4 5 2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 6
                        1 2 2 3 2 3 3 4 2 3 3 4 3 4 4 5 2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 6
                        2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 6 3 4 4 5 4 5 5 6 4 5 5 6 5 6 6 7
                        1 2 2 3 2 3 3 4 2 3 3 4 3 4 4 5 2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 6
                        2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 6 3 4 4 5 4 5 5 6 4 5 5 6 5 6 6 7
                        2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 6 3 4 4 5 4 5 5 6 4 5 5 6 5 6 6 7
                        3 4 4 5 4 5 5 6 4 5 5 6 5 6 6 7 4 5 5 6 5 6 6 7 5 6 6 7 6 7 7 8))
              (len (cdr bv)) (index (quotient len 8)) (offset (modulo len 8)))
        (do ((i 0 (+ i 1))
              (count 0 (+ count (vector-ref counts (vector-ref (car bv) i)))))
          ((= index i) count))))

    ;; Bitvector-length returns the number of bits in the bit vector:

    (define (bitvector-length bv) (cdr bv))

    ))
