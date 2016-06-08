;; -*-  scheme -*-
;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis match)
  (import (scheme base))
  (export list-match list-match-aux)

  (begin

    ;; Pattern matching provides syntactic sugar to destructure lists,
    ;; select alternatives, and bind variables, and is provided by the
    ;; list-match macro. The syntax (list-match expr clause ...) takes an
    ;; input expr that evaluates to a list. Clauses are of the form
    ;; (pattern [fender] expr), consisting of a pattern that matches a
    ;; list of a particular shape, an optional fender that must succeed if
    ;; the pattern is to match, and an expr that is evaluated if the
    ;; pattern matches. There are four types of patterns:

    ;; () — Matches the null list.

    ;; (pat0 pat1 ...) — Matches a list with length exactly equal to the
    ;; number of pattern elements.

    ;; (pat0 pat1 ... . patRest) — Matches a list with length at least as
    ;; great as the number of pattern elements before the literal dot.
    ;; PatRest is a list containing the remaining elements of the input
    ;; list after the initial prefix of the list before the literal dot.

    ;; pat — Matches an entire list. Should always appear as the last
    ;; clause; it’s not an error to appear elsewhere, but subsequent
    ;; clauses could never match.

    ;; Each pattern element may be:

    ;; An identifier — Matches any list element. Additionally, the value
    ;; of the list element is bound to the variable named by the
    ;; identifier, which is in scope in the fender and expr of the
    ;; corresponding clause. Each identifier in a single pattern must be
    ;; unique.

    ;; A literal underscore — Matches any list element, but creates no
    ;; bindings.

    ;; A constant — Matches if the expression equals the constant value,
    ;; but creates no bindings.

    ;; A quote expression — Matches if the expression equals the quote
    ;; expression, but creates no bindings.

    ;; A quasiquote expression — Matches if the expression equals the
    ;; quasiquote expression, but creates no bindings.

    ;; All comparisons are made with equal?. The patterns are tested in
    ;; order, left to right, until a matching pattern is found; if fender
    ;; is present, it must evaluate as non-#f for the match to be
    ;; successful. Pattern variables are bound in the corresponding fender
    ;; and expression. Once the matching pattern is found, the
    ;; corresponding expression is evaluated and returned as the result of
    ;; the match. An error is signaled if no pattern matches the input
    ;; list.

    ;; Pattern matching is performed by a macro that expands into a cond
    ;; expression with one clause per pattern; an auxiliary macro handles
    ;; the various types of pattern elements. The complete implementation,
    ;; which is based on an idea of Jos Koot, is given below:

    (define-syntax list-match
      (syntax-rules ()
        ((_ expr (pattern fender ... template) ...)
	 (let ((obj expr))
	   (cond ((list-match-aux obj pattern fender ...
				  (list template)) => car) ...
				  (else (error 'list-match "pattern failure")))))))

    ;; (define-syntax list-match-aux
    ;;   (lambda (stx)
    ;;     (define (underscore? x)
    ;;       (and (identifier? x) (free-identifier=? x (syntax _))))
    ;;     (syntax-case stx (quote quasiquote)
    ;;       ((_ obj pattern template)
    ;;         (syntax (list-match-aux obj pattern #t template)))
    ;;       ((_ obj () fender template)
    ;;         (syntax (and (null? obj) fender template)))
    ;;       ((_ obj underscore fender template)
    ;;         (underscore? (syntax underscore))
    ;;         (syntax (and fender template)))
    ;;       ((_ obj var fender template)
    ;;         (identifier? (syntax var))
    ;;         (syntax (let ((var obj)) (and fender template))))
    ;;       ((_ obj (quote datum) fender template)
    ;;         (syntax (and (equal? obj (quote datum)) fender template)))
    ;;       ((_ obj (quasiquote datum) fender template)
    ;;         (syntax (and (equal? obj (quasiquote datum)) fender template)))
    ;;       ((_ obj (kar . kdr) fender template)
    ;;         (syntax (and (pair? obj)
    ;;                   (let ((kar-obj (car obj)) (kdr-obj (cdr obj)))
    ;;                     (list-match-aux kar-obj kar
    ;;                       (list-match-aux kdr-obj kdr fender template))))))
    ;;       ((_ obj const fender template)
    ;;         (syntax (and (equal? obj const) fender template))))))

    ))
