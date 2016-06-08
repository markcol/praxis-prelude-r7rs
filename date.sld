;; -*-  scheme -*-
;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis date)
  (import (scheme base))
  (cond-expand
   (chibi (import (chibi time)))
   (chez  (import (chez time))))
  (export julian gregorian easter today)

  (begin
    ;; Astronomers calculate the julian number of a date as the number of
    ;; days elapsed since January 1, 4713 BC. The Gregorian calendar,
    ;; promulgated by Pope Gregory XIII on February 24, 1582, is the civil
    ;; calendar used in much of the world. Functions julian and gregorian
    ;; translate between the two calendars; year must be specified as the
    ;; full four-digit number (unless you want years in the first
    ;; millenium), month ranges from 1 for January to 12 for December, day
    ;; ranges from 1 to 31, and the day of the week can be calculated as
    ;; the julian number modulo 7, with 0 for Monday and 6 for Sunday:

    (define (julian year month day)
      (let* ((a (quotient (- 14 month) 12))
	     (y (+ year 4800 (- a)))
	     (m (+ month (* 12 a) -3)))
        (+ day
	   (quotient (+ (* 153 m) 2) 5)
	   (* 365 y)
	   (quotient y 4)
	   (- (quotient y 100))
	   (quotient y 400)
	   (- 32045))))

    (define (gregorian julian)
      (let* ((j (+ julian 32044))
	     (g (quotient j 146097))
	     (dg (modulo j 146097))
	     (c (quotient (* (+ (quotient dg 36524) 1) 3) 4))
	     (dc (- dg (* c 36524)))
	     (b (quotient dc 1461))
	     (db (modulo dc 1461))
	     (a (quotient (* (+ (quotient db 365) 1) 3) 4))
	     (da (- db (* a 365)))
	     (y (+ (* g 400) (* c 100) (* b 4) a))
	     (m (- (quotient (+ (* da 5) 308) 153) 2))
	     (d (+ da (- (quotient (* (+ m 4) 153) 5)) 122))
	     (year (+ y (- 4800) (quotient (+ m 2) 12)))
	     (month (+ (modulo (+ m 2) 12) 1))
	     (day (+ d 1)))
        (values year month day)))

    ;; For several centuries, the calculation of the date of Easter, a
    ;; calculation known as the computus, was the most important
    ;; scientific endeavor of the entire world. Function easter calculates
    ;; the julian number of the date of Easter for a given year. If offset
    ;; is given, it is the number of days before or after Easter; for
    ;; instance, to compute the date of Mardi Gras, give an offset of -47:

    (define (easter year . offset)
      (let* ((a (modulo year 19))
	     (b (quotient year 100))
	     (c (modulo year 100))
	     (d (quotient b 4))
	     (e (modulo b 4))
	     (f (quotient (+ b 8) 25))
	     (g (quotient (+ (- b f) 1) 3))
	     (h (modulo (- (+ (* 19 a) b 15) d g) 30))
	     (i (quotient c 4))
	     (k (modulo c 4))
	     (l (modulo (- (+ 32 (* 2 e) (* 2 i)) h k) 7))
	     (m (quotient (+ a (* 11 h) (* 22 l)) 451))
	     (month (quotient (- (+ h l 114) (* 7 m)) 31))
	     (day (+ (modulo (- (+ h l 114) (* 7 m)) 31) 1))
	     (q (if (pair? offset) (car offset) 0)))
        (+ (julian year month day) q)))

    ;; Calculating the current date requires help from the local Scheme
    ;; interpreter, since Scheme defines no standard functions on dates.
    ;; Shown below are versions for two popular Scheme interpreters:

    (define (today)
      (cond-expand
       (chibi
	(let ((tm (seconds->time (current-seconds))))
	  (julian (+ (time-year tm) 1900)
		  (time-month tm)
		  (time-day tm))))
       (chez
	(julian
	 (date-year (current-date))
	 (+ (date-month (current-date)) 1)
	 (date-day (current-date))))
       (else
	(error "Unknown Scheme implementation."))))

    ))
