;; -*-  scheme -*-
;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis control-flow)
  (import (scheme base))
  ;; unless, let-values, while exported by base
  (export when unless let-values while)
  (begin

    ;; While performs a block of code as long as the controlling
    ;; predicate is true.
    (define-syntax while
      (syntax-rules ()
        ((while pred? body ...)
          (do () ((not pred?)) body ...))))

    ))
