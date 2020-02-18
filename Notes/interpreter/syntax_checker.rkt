#lang racket
 
; Syntax Checker for a langauge for (operator number number)
; Example: (+ 1 2)
; Just a syntax checker, not a solver

; Helpful functions
(define (first lst) (car lst)))
(define (second lst) (cadr lst)))
(define (third lst) (cadr (cdr lst)))

; Implementation 1, more bruteforce way
(define (synchk1 expr)
  (if (number? expr)
      true
      (if (list? expr)
	  (if (equal? (length expr) 3 )
	      (if (or (equal? (car expr) `+)
		      (equal? (car expr) `-)
		      )
		  (and (synchk1 (second expr))
		       (synchk1 (third expr))
		       )
		  false)
	     false)
	  false)
      false)
  )


; Implementation with COND, which is a little more clean
(define (synchk2 expr)
  (cond
    [(number? expr) true]
    [(and (list? expt) (equal? (length expr) 3)) (if (or (equal? (first expr) `+)
							 (equal? (first expr) `-))
						     (and (synchk2 (second expr))
							  (synchk2 (third expr))
							  )
						     )
						 ]
    [else false]
    )
  )


; Implementation 3,
(define (synchk3 expr)
  (if (number? expr)
      true
      (if (list? expr)
          (if (equal? (length expr) 3 )
              (if (or (equal? (car expr) `+)
                      (equal? (car expr) `-)
                      )
		  (cons (trans (second expr))
			(cons (first expr)
			      (list (trans (third expr)))
			      )
			)
		  false)
             false)
          false)
      false)
  )

