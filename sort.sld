;; -*-  scheme -*-

;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis sort)
  (import (scheme base))
  (export sort merge unique uniq-c vector-sort!)

  (begin
    ;; Most Scheme systems provide a sort function, and it is required in
    ;; R6RS. For those that don’t, we provide this sort, which is stolen
    ;; from Kent Dybvig’s book The Scheme Programming Language; it is
    ;; stable, applicative, miserly about garbage generation, and fast,
    ;; and it also provides a merge function. Lt? is a predicate that
    ;; takes two elements of xs and returns #t if the first precedes the
    ;; second and #f otherwise:

    (define sort #f)
    (define merge #f)
    (let ()
      (define dosort
        (lambda (pred? ls n)
          (if (= n 1)
            (list (car ls))
            (let ((i (quotient n 2)))
              (domerge pred?
                (dosort pred? ls i)
                (dosort pred? (list-tail ls i) (- n i)))))))
      (define domerge
        (lambda (pred? l1 l2)
          (cond
            ((null? l1) l2)
            ((null? l2) l1)
            ((pred? (car l2) (car l1))
              (cons (car l2) (domerge pred? l1 (cdr l2))))
            (else (cons (car l1) (domerge pred? (cdr l1) l2))))))
      (set! sort
        (lambda (pred? l)
          (if (null? l) l (dosort pred? l (length l)))))
      (set! merge
        (lambda (pred? l1 l2)
          (domerge pred? l1 l2))))

    ;; Like its unix counterpart, unique returns its input list with
    ;; adjacent duplicates removed. Uniq-c returns its input list paired
    ;; with a count of adjacent duplicates, just like unix uniq with the
    ;; -c flag.

    (define (unique eql? xs)
      (cond ((null? xs) '())
        ((null? (cdr xs)) xs)
        ((eql? (car xs) (cadr xs))
          (unique eql? (cdr xs)))
        (else (cons (car xs) (unique eql? (cdr xs))))))

    (define (uniq-c eql? xs)
      (if (null? xs) xs
        (let loop ((xs (cdr xs)) (prev (car xs)) (k 1) (result '()))
          (cond ((null? xs) (reverse (cons (cons prev k) result)))
            ((eql? (car xs) prev) (loop (cdr xs) prev (+ k 1) result))
            (else (loop (cdr xs) (car xs) 1 (cons (cons prev k) result)))))))

    ;; Vectors are sorted with the Bentley/McIlroy quicksort. The
    ;; comparison function (cmp a b) returns an integer that is less than,
    ;; equal to, or greater than zero when its first argument is less
    ;; than, equal to, or greater than its second.

    (define (vector-sort! vec comp)
      (define-syntax while
        (syntax-rules ()
          ((while pred? body ...)
            (do () ((not pred?)) body ...))))
      (define-syntax assign!
        (syntax-rules ()
          ((assign! var expr)
            (begin (set! var expr) var))))

      (define len (vector-length vec))
      (define-syntax v (syntax-rules () ((v k) (vector-ref vec k))))
      (define-syntax v! (syntax-rules () ((v! k x) (vector-set! vec k x))))
      (define-syntax cmp (syntax-rules () ((cmp a b) (comp (v a) (v b)))))
      (define-syntax lt? (syntax-rules () ((lt? a b) (negative? (cmp a b)))))
      (define-syntax swap! (syntax-rules () ((swap! a b)
                                              (let ((t (v a))) (v! a (v b)) (v! b t)))))
      (define (vecswap! a b s)
        (do ((a a (+ a 1)) (b b (+ b 1)) (s s (- s 1))) ((zero? s))
          (swap! a b)))

      (define (med3 a b c)
        (if (lt? b c)
          (if (lt? b a) (if (lt? c a) c a) b)
          (if (lt? c a) (if (lt? b a) b a) c)))
      (define (pv-init a n)
        (let ((pm (+ a (quotient n 2))))
          (when (> n 7)
            (let ((pl a) (pn (+ a n -1)))
              (when (> n 40)
                (let ((s (quotient n 8)))
                  (set! pl (med3 pl (+ pl s) (+ pl s s)))
                  (set! pm (med3 (- pm s) pm (+ pm s)))
                  (set! pn (med3 (- pn s s) (- pn s) pn))))
              (set! pm (med3 pl pm pn))))
          pm))

      (let qsort ((a 0) (n len))
        (if (< n 7)
          (do ((pm (+ a 1) (+ pm 1))) ((not (< pm (+ a n))))
            (do ((pl pm (- pl 1)))
              ((not (and (> pl a) (> (cmp (- pl 1) pl) 0))))
              (swap! pl (- pl 1))))
          (let ((pv (pv-init a n)) (r #f)
                 (pa a) (pb a) (pc (+ a n -1)) (pd (+ a n -1)))
            (swap! a pv) (set! pv a)
            (let loop ()
              (while (and (<= pb pc) (<= (assign! r (cmp pb pv)) 0))
                (when (= r 0) (swap! pa pb) (set! pa (+ pa 1)))
                (set! pb (+ pb 1)))
              (while (and (>= pc pb) (>= (assign! r (cmp pc pv)) 0))
                (when (= r 0) (swap! pc pd) (set! pd (- pd 1)))
                (set! pc (- pc 1)))
              (unless (> pb pc)
                (swap! pb pc) (set! pb (+ pb 1)) (set! pc (- pc 1)) (loop)))
            (let ((pn (+ a n)))
              (let ((s (min (- pa a) (- pb pa)))) (vecswap! a (- pb s) s))
              (let ((s (min (- pd pc) (- pn pd 1)))) (vecswap! pb (- pn s) s))
              (let ((s (- pb pa))) (when (> s 1) (qsort a s)))
              (let ((s (- pd pc))) (when (> s 1) (qsort (- pn s) s))))))))

    ))
