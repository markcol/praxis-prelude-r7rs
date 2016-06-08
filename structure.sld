;; -*-  scheme -*-
;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis structure)
  (import (scheme base))
  (export define-structure)

  (begin
    ;; R5RS Scheme does not provide any way to create new variable types
    ;; or to combine multiple variables into a single unit; R6RS provides
    ;; structures that perform these tasks. The structures given below are
    ;; upward-compatible to R6RS structures. Calling (define-structure
    ;; name field ...) creates a new structure. Define-structure expands
    ;; into a constructor (make-name field ...), a type predicate (name?
    ;; obj), and an accessor (name-field x) and setter (set-name-field! x
    ;; value) for an object x of structure type name.

    ;; (define-syntax (define-structure x)
    ;;   (define (gen-id template-id . args)
    ;;     (datum->syntax-object template-id
    ;;       (string->symbol
    ;;         (apply string-append
    ;;           (map (lambda (x)
    ;;                  (if (string? x) x
    ;;                    (symbol->string
    ;;                      (syntax-object->datum x))))
    ;;             args)))))
    ;;   (syntax-case x ()
    ;;     ((_ name field ...)
    ;;       (with-syntax
    ;;         ((constructor (gen-id (syntax name) "make-" (syntax name)))
    ;;           (predicate (gen-id (syntax name) (syntax name) "?"))
    ;;           ((access ...)
    ;;             (map (lambda (x) (gen-id x (syntax name) "-" x))
    ;;               (syntax (field ...))))
    ;;           ((assign ...)
    ;;             (map (lambda (x) (gen-id x "set-" (syntax name) "-" x "!"))
    ;;               (syntax (field ...))))
    ;;           (structure-length (+ (length (syntax (field ...))) 1))
    ;;           ((index ...) (let f ((i 1) (ids (syntax (field ...))))
    ;;                          (if (null? ids) '()
    ;;                            (cons i (f (+ i 1) (cdr ids)))))))
    ;;         (syntax (begin
    ;;                   (define (constructor field ...)
    ;;                     (vector 'name field ...))
    ;;                   (define (predicate x)
    ;;                     (and (vector? x)
    ;;                       (= (vector-length x) structure-length)
    ;;                       (eq? (vector-ref x 0) 'name)))
    ;;                   (define (access x) (vector-ref x index)) ...
    ;;                   (define (assign x update) (vector-set! x index update))
    ;;                   ...))))))

    ))
