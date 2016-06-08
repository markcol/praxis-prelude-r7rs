;; -*-  scheme -*-
;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis generator)
  (import (scheme base))
  (export define-generator)

  (begin
    ;; Generators provide an easy-to-use syntax for separating the
    ;; production of values from their consumption, and are provided
    ;; natively in many other languages (sometimes they are called
    ;; iterators). A function defined by define-generator creates a
    ;; function that, when called, returns the next value in a sequence.
    ;; For instance:

    ;; > (define-generator (yield123)
    ;;     (yield 1) (yield 2) (yield 3))
    ;; > (define y (yield123))
    ;; > (y)
    ;; 1
    ;; > (y)
    ;; 2
    ;; > (y)
    ;; 3
    ;; > (y)
    ;; Exception in yield123: unexpected return

    ;; (define-syntax define-generator
    ;;   (lambda (x)
    ;;     (syntax-case x (lambda)
    ;;       ((stx name (lambda formals e0 e1 ...))
    ;;         (with-syntax ((yield (datum->syntax (syntax stx) 'yield)))
    ;;           (syntax (define name
    ;;                     (lambda formals
    ;;                       (let ((resume #f) (return #f))
    ;;                         (define yield
    ;;                           (lambda args
    ;;                             (call-with-current-continuation
    ;;                               (lambda (cont)
    ;;                                 (set! resume cont)
    ;;                                 (apply return args)))))
    ;;                         (lambda ()
    ;;                           (call-with-current-continuation
    ;;                             (lambda (cont)
    ;;                               (set! return cont)
    ;;                               (cond (resume (resume))
    ;;                                 (else (let () e0 e1 ...)
    ;;                                   (error 'name "unexpected return"))))))))))))
    ;;       ((stx (name . formals) e0 e1 ...)
    ;;         (syntax (stx name (lambda formals e0 e1 ...)))))))

    ))
