;; -*-  scheme -*-
;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis misc)
  (import (scheme base))
  (export box box! gensym  permutations unbox
	  ;; define-integrable define-macro aif awhen
	  ;; provided by scheme base:
	  when)
  
  (begin
    ;; Define-integrable is similar to define for procedure definitions
    ;; except that the code for the procedure is integrated (some people
    ;; would say inlined) whenever the procedure is called, eliminating
    ;; the function-call overhead associated with the procedure. Any
    ;; procedure defined with define-integrable must appear in the source
    ;; code before the first reference to the defined identifier. Lexical
    ;; scoping is preserved, macros within the body of the defined
    ;; procedure are expanded at the point of call, the actual parameters
    ;; to an integrated procedure are evaluated once and at the proper
    ;; time, integrable procedures may be used as first-class values, and
    ;; recursive procedures do not cause indefinite recursive expansion.
    ;; Define-integrable appears in Section 8.4 of R. Kent Dybvig’s book
    ;; The Scheme Programming Language:

    ;; (define-syntax (define-integrable x)
    ;;   (define (make-residual-name name)
    ;;     (datum->syntax-object name
    ;;       (string->symbol
    ;;         (string-append "residual-"
    ;;           (symbol->string (syntax-object->datum name))))))
    ;;   (syntax-case x (lambda)
    ;;     ((_ (name . args) . body)
    ;;       (syntax (define-integrable name (lambda args . body))))
    ;;     ((_ name (lambda formals form1 form2 ...))
    ;;       (identifier? (syntax name))
    ;;       (with-syntax ((xname (make-residual-name (syntax name))))
    ;;         (syntax
    ;;           (begin
    ;;             (define-syntax (name x)
    ;;               (syntax-case x ()
    ;;                 (_ (identifier? x) (syntax xname))
    ;;                 ((_ arg (... ...))
    ;;                   (syntax
    ;;                     ((fluid-let-syntax
    ;;                        ((name (identifier-syntax xname)))
    ;;                        (lambda formals form1 form2 ...))
    ;;                       arg (... ...))))))
    ;;             (define xname
    ;;               (fluid-let-syntax ((name (identifier-syntax xname)))
    ;;                 (lambda formals form1 form2 ...)))))))))

    ;; Scheme provides hygienic macros (though syntax-case provides a way
    ;; to safely bend hygiene); Common Lisp, by comparison, provides
    ;; unhygienic macros. There are some circumstances where unhygienic
    ;; macros are more convenient than hygienic macros; Paul Graham
    ;; provides numerous examples in his book On Lisp. Define-macro
    ;; provides unhygienic macros for Scheme:

    ;; (define-syntax (define-macro x)
    ;;   (syntax-case x ()
    ;;     ((_ (name . args) . body)
    ;;       (syntax (define-macro name (lambda args . body))))
    ;;     ((_ name transformer)
    ;;       (syntax
    ;;         (define-syntax (name y)
    ;;           (syntax-case y ()
    ;;             ((_ . args)
    ;;               (datum->syntax-object
    ;;                 (syntax _)
    ;;                 (apply transformer
    ;;                   (syntax-object->datum (syntax args)))))))))))

    ;; The following examples are adapted from Graham’s book:

    ;; (define-macro (aif test-form then-else-forms)
    ;;   `(let ((it ,test-form))
    ;;      (if it ,then-else-forms)))

    ;; (define-macro (awhen pred? . body)
    ;;   `(aif ,pred? (begin ,@body)))

    ;; When a macro breaks hygiene, it is sometimes useful to generate a
    ;; unique symbol, which can be used as a variable name or in some
    ;; other way. Here is the gensym procedure:

    (define gensym
      (let ((n -1))
        (lambda ()
          (set! n (+ n 1))
          (string->symbol
            (string-append "gensym-"
              (number->string n))))))

    ;; Boxes provide a way to pass arguments to procedures by reference
    ;; instead of the usual passing by value; put the argument in a box,
    ;; then access and reset the argument in the procedure:

    (define (box v) (vector v))
    (define (unbox box) (vector-ref box 0))
    (define (box! box v) (vector-set! box 0 v))

    ;; It is sometimes useful to generate a list of the permutations of a
    ;; list. The function below is from Shmuel Zaks, A new algorithm for
    ;; generation of permutations, Technical Report 220, Technion-Israel
    ;; Institute of Technology, 1981:

    (define (permutations xs)
      (define (rev xs n ys)
        (if (zero? n) ys
          (rev (cdr xs) (- n 1) (cons (car xs) ys))))
      (let ((xs xs) (perms (list xs)))
        (define (perm n)
          (if (> n 1)
            (do ((j (- n 1) (- j 1)))
              ((zero? j) (perm (- n 1)))
              (perm (- n 1))
              (set! xs (rev xs n (list-tail xs n)))
              (set! perms (cons xs perms)))))
        (perm (length xs))
        perms))

    ))
