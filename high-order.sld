;; -*-  scheme -*-

;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis high-order)
  (import (scheme base))
  (export
    identity constant fst snd compose complement swap
    left-section right-section curried-lambda define-curried
    )

  (begin
    ;; Identity returns it only argument and constant returns a function
    ;; that, when called, always returns the same value regardless of its
    ;; argument. Both functions are useful as recursive bases when writing
    ;; higher-order functions.

    (define (identity x) x)

    (define (constant x) (lambda ys x))

    ;; Fst returns it first argument and snd returns its second argument;
    ;; both functions occasionally find use when composing strings of
    ;; functions:

    (define (fst x y) x)

    (define (snd x y) y)

    ;; Function composition creates a new function by partially applying
    ;; multiple functions, one after the other. In the simplest case,
    ;; there are only two functions, f and g, composed as ((compose f g)
    ;; x) ≡ (f (g x)); the composition can be bound to create a new
    ;; function, as in (define fg (compose f g)). Compose takes one or
    ;; more procedures and returns a new procedure that performs the same
    ;; action as the individual procedures would if called in succession.

    (define (compose . fns)
      (let comp ((fns fns))
        (cond
          ((null? fns) 'error)
          ((null? (cdr fns)) (car fns))
          (else
            (lambda args
              (call-with-values
                (lambda ()
                  (apply
                    (comp (cdr fns))
                    args))
                (car fns)))))))

    ;; Complement takes a predicate and returns a new predicate that
    ;; returns #t where the original returned #f and #f where the original
    ;; returned non-#f. It is useful with functions like filter and
    ;; take-while that take predicates as arguments.

    (define (complement f) (lambda xs (not (apply f xs))))

    ;; Swap takes a binary function and returns a new function that is
    ;; similar to the original but with arguments reversed. It is useful
    ;; when composing or currying functions that take their arguments in
    ;; an inconvenient order.

    (define (swap f) (lambda (x y) (f y x)))

    ;; A section is a procedure which has been partially applied to some
    ;; of its arguments; for instance, (double x), which returns twice its
    ;; argument, is a partial application of the multiply operator to the
    ;; number 2. Sections come in two kinds: left sections partially apply
    ;; arguments starting from the left, and right sections partially
    ;; apply arguments starting from the right. Left-section takes a
    ;; procedure and some prefix of its arguments and returns a new
    ;; procedure in which those arguments are partially applied.
    ;; Right-section takes a procedure and some reversed suffix of its
    ;; arguments and returns a new procedure in which those arguments are
    ;; partially applied.

    (define (left-section proc . args)
      (lambda xs (apply proc (append args xs))))

    (define (right-section proc . args)
      (lambda xs (apply proc (reverse (append (reverse args) (reverse xs))))))

    ;; Currying is the technique of rewriting a function that takes
    ;; multiple arguments so that it can be called as a chain of functions
    ;; that each take a single argument; the technique is named after the
    ;; mathematician Haskell Curry, who discovered it (Moses Schönfinkel
    ;; discovered the technique independently, but the term schönfinkeling
    ;; never caught on). For example, if div is the curried form of the
    ;; division operator, defined as (define-curried (div x y) (/ x y)),
    ;; then inv is the function that returns the inverse of its argument,
    ;; defined as (define inv (div 1)).

    (define-syntax curried-lambda
      (syntax-rules ()
        ((_ () body body* ...)
          (begin body body* ...))
        ((_ (arg arg* ...) body body* ...)
          (lambda (arg)
            (curried-lambda (arg* ...)
              body body* ...)))))

    (define-syntax define-curried
      (syntax-rules ()
        ((_ (func arg ...) body body* ...)
          (define func
            (curried-lambda (arg ...)
              body body* ...)))))

    ))
