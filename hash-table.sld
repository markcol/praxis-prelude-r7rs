;; -*-  scheme -*-

;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis hash-table)
  (import (scheme base)
          (scheme cxr))
  (export make-hash string-hash list->hash)

  (begin
    ;; Hash tables are one of the greatest inventions of computer
    ;; science, permitting very fast retrieval of key/value pairs.

    ;; Make-hash creates an instance of an abstract data type of hash
    ;; tables. Make-hash takes four arguments: Hash is a function that
    ;; takes a key and returns an integer that provides an address for
    ;; the bucket where the key/value pair is stored. Eql? is a
    ;; predicate that takes two keys and returns #t if they are the
    ;; same and #f otherwise. Oops is the default value returned by
    ;; the hash table if a requested key is not present in the table.
    ;; Size is the number of buckets where key/value pairs are stored;
    ;; it is best to choose size as a prime number of magnitude
    ;; similar to the expected number of key/value pairs. Make-hash
    ;; returns a function that, when called with an appropriate
    ;; message, performs the requested action; for a hash table
    ;; created by

    ;; (define state-tab (make-hash string-hash string=? #f 4093))

    ;; the appropriate call is

    ;; (state-tab 'message args ...)

    ;; where message and args can be any of the following:

    ;; insert key value— inserts a key/value pair in the hash table,
    ;; overwriting any previous value associated with the keyp

    ;; lookup key — retrieves the value associated with key

    ;; delete key — removes key and its associated value from the hash
    ;; table, if it exists

    ;; update key proc default — proc is a function that takes a key
    ;; and value as arguments and returns a new value; if key is
    ;; present in the hash table, update calls proc with the key and
    ;; its associated value and stores the value returned by proc in
    ;; place of the original value, otherwise update inserts a new
    ;; key/value pair in the hash table with key key> and value
    ;; default.

    ;; enlist — returns all the key/value pairs in the hash table as a list

    ;; Synonyms are provided for some of the messages; see the source code for details.

    (define (make-hash hash eql? oops size)
      (let ((table (make-vector size '())))
        (lambda (message . args)
          (if (eq? message 'enlist)
            (let loop ((k 0) (result '()))
              (if (= size k)
                result
                (loop (+ k 1) (append (vector-ref table k) result))))
            (let* ((key (car args))
                    (index (modulo (hash key) size))
                    (bucket (vector-ref table index)))
              (case message
                ((lookup fetch get ref recall)
                  (let loop ((bucket bucket))
                    (cond ((null? bucket) oops)
                      ((eql? (caar bucket) key) (cdar bucket))
                      (else (loop (cdr bucket))))))
                ((insert insert! ins ins! set set! store store! install install!)
                  (vector-set! table index
                    (let loop ((bucket bucket))
                      (cond ((null? bucket)
                              (list (cons key (cadr args))))
                        ((eql? (caar bucket) key)
                          (cons (cons key (cadr args)) (cdr bucket)))
                        (else (cons (car bucket) (loop (cdr bucket))))))))
                ((delete delete! del del! remove remove!)
                  (vector-set! table index
                    (let loop ((bucket bucket))
                      (cond ((null? bucket) '())
                        ((eql? (caar bucket) key)
                          (cdr bucket))
                        (else (cons (car bucket) (loop (cdr bucket))))))))
                ((update update!)
                  (vector-set! table index
                    (let loop ((bucket bucket))
                      (cond ((null? bucket)
                              (list (cons key (caddr args))))
                        ((eql? (caar bucket) key)
                          (cons (cons key ((cadr args) key (cdar bucket))) (cdr bucket)))
                        (else (cons (car bucket) (loop (cdr bucket))))))))
                (else (error 'hash-table "unrecognized message")) ))))))

    ;; The most common data type used for hash-table keys is character
    ;; strings, for which we provide this hash function:

    (define (string-hash str)
      (let loop ((cs (string->list str)) (s 0))
        (if (null? cs) s
          (loop (cdr cs) (+ (* s 31)
                           (char->integer (car cs)))))))

    ;; The list->hash function returns a newly-allocated hash table
    ;; containing each of the key/value pairs in the input list, where
    ;; the key is the car of the item and the value is the cdr of the
    ;; item:

    (define (list->hash hash eql? oops size xs)
      (let ((table (make-hash hash eql? oops size)))
        (do ((xs xs (cdr xs))) ((null? xs) table)
          (table 'insert (caar xs) (cdar xs)))))

    ))
