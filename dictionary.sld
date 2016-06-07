;; -*-  scheme -*-

;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis dictionary)
  (import (scheme base)
          (rnrs syntax-case))
  (export make-dict tree key val lkid ht size bal nil nil? rot-left rot-right
          balance lookup insert update delete-successor delete nth rank avl-map
          avl-fold avl-for-each to-list from-list make-gen new dispatch)

  (begin
    ;; Make-dict provides the abstract data type of an ordered map,
    ;; sometimes called a dictionary. Unlike hash tables that only
    ;; take an equality predicate, the dictionary takes a less-than
    ;; predicate lt? so that it can iterate over its key/value pairs
    ;; in order. The dictionary also provides order statistics, so you
    ;; can select the nth key/value pair in order or find the ordinal
    ;; rank of a given key. The implementation uses avl trees, so any
    ;; access to a particular key/value pair takes time O(log n). A
    ;; dictionary is created by:

    ;; (make-dict lt?) — returns a newly-allocated, empty dictionary
    ;; that obeys the less-than lt? ordering predicate

    ;; A dictionary is represented by a function, and once it has been
    ;; created, say by (define dict (make-dict string<?)), operators
    ;; are applied in message-passing style, such as (dict 'message
    ;; args ...) where messageand args can be any of the following:

    ;; empty? — returns #t if the dictionary is empty, else #f [nil?]

    ;; lookup key &dash; returns a (key . value) pair corresponding to
    ;; key, or #f if key is not present in the dictionary [fetch, get]

    ;; insert key value — returns a newly-allocated dictionary that
    ;; includes all the key/value pairs in the input dictionary plus
    ;; the input key and value, which replaces any existing key/value
    ;; pair with a matching key; duplicate keys are not permitted
    ;; [store, put]

    ;; update proc key value) — returns a newly-allocated dictionary;
    ;; if key is already present in the dictionary, the value
    ;; associated with the key is replaced by (proc k v), where k and
    ;; v are the existing key and value; otherwise, the input
    ;; key/value pair is added to the dictionary

    ;; delete key — returns a newly-allocated dictionary in which key
    ;; is not present, whether or not it is already present [remove]

    ;; size — returns the number of key/value pairs in the dictionary [count, length]

    ;; nth n — returns the nth key/value pair in the dictionary, counting from zero

    ;; rank key — returns the ordinal position of key in the dictionary, counting from zero

    ;; map proc — returns a newly-allocated dictionary in which each
    ;; value is replaced by (proc k v), where k and v are the existing
    ;; key and value

    ;; fold proc base &mdsah; returns the value accumulated by
    ;; applying the function (proc k v b) to each key/value pair in
    ;; the dictionary, accumulating the value of the base b at each
    ;; step; pairs are accessed in order of ascending keys

    ;; for-each proc — evaluates for its side-effects only (proc k v)
    ;; for each key/value pair in the dictionary in ascending order

    ;; to-list — returns a newly-allocated list of all the (key .
    ;; value) pairs in the dictionary, in ascending order [enlist]

    ;; from-list xs — returns a newly-allocated dictionary that
    ;; includes all the key/value pairs in the original dictionary
    ;; plus all the (key . value) pairs in xs; any key in xs that
    ;; already exists in the dictionary has its value replaced by the
    ;; corresponding value in xs

    ;; make-gen — returns a function that, each time it is called,
    ;; returns the next (key . value) pair from the dictionary, in
    ;; ascending order; when the key/value pairs in the dictionary are
    ;; exhausted, the function returns #f each time it is called [gen]

    ;; Synonyms are provided for some of the operations, as given in
    ;; square brackets above.

    (define (make-dict lt?)

      (define-syntax define-generator
        (lambda (x)
          (syntax-case x (lambda)
            ((stx name (lambda formals e0 e1 ...))
              (with-syntax ((yield (datum->syntax-object (syntax stx) 'yield)))
                (syntax (define name
                          (lambda formals
                            (let ((resume #f) (return #f))
                              (define yield
                                (lambda args
                                  (call-with-current-continuation
                                    (lambda (cont)
                                      (set! resume cont)
                                      (apply return args)))))
                              (lambda ()
                                (call-with-current-continuation
                                  (lambda (cont)
                                    (set! return cont)
                                    (cond (resume (resume))
                                      (else (let () e0 e1 ...)
                                        (error 'name "unexpected return"))))))))))))
            ((stx (name . formals) e0 e1 ...)
              (syntax (stx name (lambda formals e0 e1 ...)))))))

      (define (tree k v l r)
        (vector k v l r (+ (max (ht l) (ht r)) 1)
          (+ (size l) (size r) 1)))
      (define (key t) (vector-ref t 0))
      (define (val t) (vector-ref t 1))
      (define (lkid t) (vector-ref t 2))
      (define (rkid t) (vector-ref t 3))
      (define (ht t) (vector-ref t 4))
      (define (size t) (vector-ref t 5))
      (define (bal t) (- (ht (lkid t)) (ht (rkid t))))
      (define nil (vector 'nil 'nil 'nil 'nil 0 0))
      (define (nil? t) (eq? t nil))

      (define (rot-left t)
        (if (nil? t) t
          (tree (key (rkid t))
            (val (rkid t))
            (tree (key t) (val t) (lkid t) (lkid (rkid t)))
            (rkid (rkid t)))))

      (define (rot-right t)
        (if (nil? t) t
          (tree (key (lkid t))
            (val (lkid t))
            (lkid (lkid t))
            (tree (key t) (val t) (rkid (lkid t)) (rkid t)))))

      (define (balance t)
        (let ((b (bal t)))
          (cond ((< (abs b) 2) t)
            ((positive? b)
              (if (< -1 (bal (lkid t))) (rot-right t)
                (rot-right (tree (key t) (val t)
                             (rot-left (lkid t)) (rkid t)))))
            ((negative? b)
              (if (< (bal (rkid t)) 1) (rot-left t)
                (rot-left (tree (key t) (val t)
                            (lkid t) (rot-right (rkid t)))))))))

      (define (lookup t k)
        (cond ((nil? t) #f)
          ((lt? k (key t)) (lookup (lkid t) k))
          ((lt? (key t) k) (lookup (rkid t) k))
          (else (cons k (val t)))))

      (define (insert t k v)
        (cond ((nil? t) (tree k v nil nil))
          ((lt? k (key t))
            (balance (tree (key t) (val t)
                       (insert (lkid t) k v) (rkid t))))
          ((lt? (key t) k)
            (balance (tree (key t) (val t)
                       (lkid t) (insert (rkid t) k v))))
          (else (tree k v (lkid t) (rkid t)))))

      (define (update t f k v)
        (cond ((nil? t) (tree k v nil nil))
          ((lt? k (key t))
            (balance (tree (key t) (val t)
                       (update (lkid t) f k v) (rkid t))))
          ((lt? (key t) k)
            (balance (tree (key t) (val t)
                       (lkid t) (update (rkid t) f k v))))
          (else (tree k (f k (val t)) (lkid t) (rkid t)))))

      (define (delete-successor t)
        (if (nil? (lkid t)) (values (rkid t) (key t) (val t))
          (call-with-values
            (lambda () (delete-successor (lkid t)))
            (lambda (l k v)
              (values (balance (tree (key t) (val t) l (rkid t))) k v)))))

      (define (delete t k)
        (cond ((nil? t) nil)
          ((lt? k (key t))
            (balance (tree (key t) (val t)
                       (delete (lkid t) k) (rkid t))))
          ((lt? (key t) k)
            (balance (tree (key t) (val t)
                       (lkid t) (delete (rkid t) k))))
          ((nil? (lkid t)) (rkid t))
          ((nil? (rkid t)) (lkid t))
          (else (call-with-values
                  (lambda () (delete-successor (rkid t)))
                  (lambda (r k v) (balance (tree k v (lkid t) r)))))))

      (define (nth t n)
        (if (negative? n) (error 'nth "must be non-negative")
          (let ((s (size (lkid t))))
            (cond ((< n s) (nth (lkid t) n))
              ((< s n) (nth (rkid t) (- n s 1)))
              ((nil? t) #f)
              (else (cons (key t) (val t)))))))

      (define (rank t k)
        (let loop ((t t) (s (size (lkid t))))
          (cond ((nil? t) #f)
            ((lt? k (key t))
              (loop (lkid t) (size (lkid (lkid t)))))
            ((lt? (key t) k)
              (loop (rkid t) (+ s (size (lkid (rkid t))) 1)))
            (else s))))

      (define (avl-map proc t)          ; (proc key value)
        (if (nil? t) nil
          (tree (key t) (proc (key t) (val t))
            (avl-map proc (lkid t))
            (avl-map proc (rkid t)))))

      (define (avl-fold proc base t)    ; (proc key value base)
        (if (nil? t) base
          (avl-fold proc
            (proc (key t) (val t)
              (avl-fold proc base (lkid t)))
            (rkid t))))

      (define (avl-for-each proc t)     ; (proc key value)
        (unless (nil? t)
          (avl-for-each proc (lkid t))
          (proc (key t) (val t))
          (avl-for-each proc (rkid t))))

      (define (to-list t)
        (cond ((nil? t) (list))
          ((and (nil? (lkid t)) (nil? (rkid t)))
            (list (cons (key t) (val t))))
          (else (append (to-list (lkid t))
                  (list (cons (key t) (val t)))
                  (to-list (rkid t))))))

      (define (from-list t xs)
        (let loop ((xs xs) (t t))
          (if (null? xs) t
            (loop (cdr xs) (insert t (caar xs) (cdar xs))))))

      (define-generator (make-gen t)
        (avl-for-each (lambda (k v) (yield (cons k v))) t)
        (do () (#f) (yield #f)))

      (define (new dict)
        (lambda (message . args) (dispatch dict message args)))

      (define (dispatch dict message args)
        (define (arity n)
          (if (not (= (length args) n)) (error 'dict "incorrect arity")))
        (case message
          ((empty? nil?) (arity 0) (nil? dict))
          ((lookup fetch get) (arity 1) (apply lookup dict args))
          ((insert store put) (arity 2) (new (apply insert dict args)))
          ((update) (arity 3) (new (apply update dict args)))
          ((delete remove) (arity 1) (new (apply delete dict args)))
          ((size count length) (arity 0) (size dict))
          ((nth) (arity 1) (apply nth dict args))
          ((rank) (arity 1) (apply rank dict args))
          ((map) (arity 1) (new (avl-map (car args) dict)))
          ((fold) (arity 2) (avl-fold (car args) (cadr args) dict))
          ((for-each) (arity 1) (avl-for-each (car args) dict))
          ((to-list enlist) (arity 0) (to-list dict))
          ((from-list) (arity 1) (new (apply from-list dict args)))
          ((make-gen gen) (arity 0) (make-gen dict))
          (else (error 'dict "invalid message"))))

      (vector-set! nil 2 nil) (vector-set! nil 3 nil) (new nil))

    ))
