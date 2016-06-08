;; -*-  scheme -*-
;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis io)
  (import (scheme base)
          (scheme file))
  (export read-file for-each-input map-input fold-input filter-input)

  (begin
    
    ;; Read-file returns a list of the characters in a file:
    (define (read-file file-name)
      (with-input-from-file file-name
	(lambda ()
	  (let loop ((c (read-char)) (cs '()))
	    (if (eof-object? c) (reverse cs)
		(loop (read-char) (cons c cs)))))))

    ;; The three input processors for-each-input, map-input and fold-input
    ;; operate on an input file or port in a manner similar to the way
    ;; for-each, map and fold-left operate on lists. All three take an
    ;; optional final argument. If the final argument is missing, input is
    ;; taken from the current input port; as a side effect, all characters
    ;; on the port are exhausted. If the final argument is a port, input
    ;; is taken from the port, which is left open when the function
    ;; returns; as a side effect, all characters on the port are
    ;; exhausted. If the final argument is a string, it is taken as the
    ;; name of a file which is opened, used as the source of input, and
    ;; closed before the function returns. For all three functions, reader
    ;; is a function that returns the next item from the input, and proc
    ;; is a function that operates on each item.

    (define (for-each-input reader proc . pof)
      (let* ((f? (and (pair? pof) (string? (car pof))))
	     (p (cond (f? (open-input-file (car pof)))
		      ((pair? pof) (car pof))
		      (else (current-input-port)))))
        (do ((item (reader p) (reader p)))
	    ((eof-object? item)
	     (if f? (close-input-port p)))
          (proc item))))

    (define (map-input reader proc . pof)
      (let* ((f? (and (pair? pof) (string? (car pof))))
	     (p (cond (f? (open-input-file (car pof)))
		      ((pair? pof) (car pof))
		      (else (current-input-port)))))
        (let loop ((item (reader p)) (result '()))
          (if (eof-object? item)
	      (begin (if f? (close-input-port p)) (reverse result))
	      (loop (reader p) (cons (proc item) result))))))

    (define (fold-input reader proc base . pof)
      (let* ((f? (and (pair? pof) (string? (car pof))))
	     (p (cond (f? (open-input-file (car pof)))
		      ((pair? pof) (car pof))
		      (else (current-input-port)))))
        (let loop ((item (reader p)) (base base))
          (if (eof-object? item)
	      (begin (if f? (close-input-port p)) base)
	      (loop (reader p) (proc base item))))))

    ;; Filter-input is a combinator that takes a reader function and a
    ;; predicate and returns a new reader function that only passes
    ;; input items x for which (pred? x) is non-#f.

    (define (filter-input reader pred?)
      (lambda args
        (let loop ((item (apply reader args)))
          (if (or (eof-object? item) (pred? item)) item
	      (loop (apply reader args))))))

    ))
