;; -*-  scheme -*-

;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis random)
  (import (scheme base)
    (scheme cxr))
  (export rand randint mod-diff flip-cycle init-rand next-rand unif-rand
          fortune shuffle)

  (begin
    ;; This is the portable, high-quality random number generator provided
    ;; in the Stanford GraphBase by Donald E. Knuth. Based on the lagged
    ;; fibonacci generator (Knuth calls it the subtractive method)an =
    ;; (an-24 − an-55) mod m, where m is an even number (we take m = 231)
    ;; and the numbers a0 through a54 are not all even, it provides values
    ;; on the range zero inclusive to 231 exclusive, has a period of at
    ;; least 255 − 1 but is plausibly conjectured to have a period of 285
    ;; − 230 for all but at most one choice of the seed value, and the
    ;; low-order bits of the generated numbers are just as random as the
    ;; high-order bits. You can read Knuth’s original version of the
    ;; random number generator at http://tex.loria.fr/sgb/gb_flip.pdf; see
    ;; also our exercise GB_FLIP. This random number generator is suitable
    ;; for simulation but is not cryptographically secure.

    ;; Called with no arguments, (rand) returns an exact rational number
    ;; on the range zero inclusive to one exclusive. Called with a single
    ;; numeric argument, (rand seed) resets the seed of the random number
    ;; generator; it is best if the seed is a large integer (eight to ten
    ;; digits — dates in the form YYYYMMDD work well), though seeds like
    ;; 0.3 (three-tenths of 2^35) and -42 (2^35-42) also work. Since the
    ;; shuffling algorithm requires its own data store, knowing the
    ;; current seed is not sufficient to restart the generator. Thus,
    ;; (rand 'get) returns the complete current state of the generator,
    ;; and (rand 'set state) resets the generator to the given state,
    ;; given a state in the form provided by (rand 'get).

    ;; (randint n) returns a random non-negative integer less than n.

    ;; (randint first past) returns a random integer between first
    ;; inclusive and past exclusive.

    ;; Two versions of the mod-diff function are included. You should use
    ;; the fast version if your Scheme provides logand natively, or the
    ;; generic version otherwise. Note that logand is sometimes provided
    ;; under a different name; for instance, it is bitwise-and in R6RS.

    (define rand #f)
    (define randint #f)
    (define flip-cycle #f)
    (define mod-diff #f)
    (define init-rand #f)
    (define next-rand #f)
    (define unif-rand #f)
    (let ( (two31 #x80000000)
           (a     (make-vector 56 -1))
           (fptr  #f) )

      (define (mod-diff x y) (modulo (- x y) two31)) ; generic version
      ;; (define (mod-diff x y) (logand (- x y) #x7FFFFFFF)) ; fast version

      (define (flip-cycle)
        (do ((ii 1 (+ ii 1)) (jj 32 (+ jj 1))) ((< 55 jj))
          (vector-set! a ii (mod-diff (vector-ref a ii) (vector-ref a jj))))
        (do ((ii 25 (+ ii 1)) (jj 1 (+ jj 1))) ((< 55 ii))
          (vector-set! a ii (mod-diff (vector-ref a ii) (vector-ref a jj))))
        (set! fptr 54) (vector-ref a 55))

      (define (init-rand seed)
        (let* ((seed (mod-diff seed 0)) (prev seed) (next 1))
          (vector-set! a 55 prev)
          (do ((i 21 (modulo (+ i 21) 55))) ((zero? i))
            (vector-set! a i next) (set! next (mod-diff prev next))
            (set! seed (+ (quotient seed 2) (if (odd? seed) #x40000000 0)))
            (set! next (mod-diff next seed)) (set! prev (vector-ref a i)))
          (flip-cycle) (flip-cycle) (flip-cycle) (flip-cycle) (flip-cycle)))

      (define (next-rand)
        (if (negative? (vector-ref a fptr)) (flip-cycle)
          (let ((next (vector-ref a fptr))) (set! fptr (- fptr 1)) next)))

      (define (unif-rand m)
        (let ((t (- two31 (modulo two31 m))))
          (let loop ((r (next-rand)))
            (if (<= t r) (loop (next-rand)) (modulo r m)))))

      (init-rand 19380110)             ; happy birthday donald e knuth

      (set! rand (lambda seed
                   (cond ((null? seed) (/ (next-rand) two31))
                     ((eq? (car seed) 'get) (cons fptr (vector->list a)))
                     ((eq? (car seed) 'set) (set! fptr (caadr seed))
                       (set! a (list->vector (cdadr seed))))
                     (else (/ (init-rand (modulo (numerator
                                                   (exact (car seed))) two31)) two31)))))
      (set! randint (lambda args
                      (cond ((null? (cdr args))
                              (if (< (car args) two31) (unif-rand (car args))
                                (floor (* (next-rand) (car args)))))
                        ((< (car args) (cadr args))
                          (let ((span (- (cadr args) (car args))))
                            (+ (car args)
                              (if (< span two31) (unif-rand span)
                                (floor (* (next-rand) span))))))
                        (else (let ((span (- (car args) (cadr args))))
                                (- (car args)
                                  (if (< span two31) (unif-rand span)
                                    (floor (* (next-rand) span))))))))))

    ;; Fortune selects an item randomly from a list; the first item is
    ;; selected with probability 1/1, the second item replaces the
    ;; selection with probability 1/2, the third item replaces that
    ;; selection with probability 1/3, and so on, so that the kth item is
    ;; selected with probability 1/k. The name derives from the unix game
    ;; of the same name, which selects an epigram randomly from a file
    ;; containing one per line.

    (define (fortune xs)
      (let loop ((n 1) (x #f) (xs xs))
        (cond ((null? xs) x)
          ((< (rand) (/ n))
            (loop (+ n 1) (car xs) (cdr xs)))
          (else (loop (+ n 1) x (cdr xs))))))

    ;; To shuffle a list, convert it to a vector, shuffle the vector by
    ;; Knuth’s algorithm, and convert the result back to a list:

    (define (shuffle x)
      (do ((v (list->vector x)) (n (length x) (- n 1)))
        ((zero? n) (vector->list v))
        (let* ((r (randint n)) (t (vector-ref v r)))
          (vector-set! v r (vector-ref v (- n 1)))
          (vector-set! v (- n 1) t))))

    ))
