;; -*-  scheme -*-

;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis assert)
  (import (scheme base))
  (export assert)

  (begin
    ;; The assert macro is useful when testing programs. The syntax
    ;; (assert expr result) computes expr and result; if they are the
    ;; same, assert produces no output and returns no value. But if expr
    ;; and result differ, assert writes a message that includes the text
    ;; of expr and the result of computing both expr and result. Assert is
    ;; a macro, not a function, because it prints the literal expr as part
    ;; of its output, making it easy in a long sequence of assertions to
    ;; know which is in error. Assert produces no output if all is well,
    ;; on the theory that “No news is good news.”

    (define-syntax assert
      (syntax-rules ()
        ((assert expr result)
          (if (not (equal? expr result))
            (for-each display `(
                                 #\newline "failed assertion:" #\newline
                                 expr #\newline "expected: " ,result
                                 #\newline "returned: " ,expr #\newline))))))

    ))
