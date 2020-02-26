#lang racket

(provide (all-defined-out))
(require racket/trace)
(require "PTS.rkt")

#| PROD |#

#|-------------------------------Helper Functions-----------------------------|#
(define (square x) (* x x ))  ; return x^2
(define (length lst result)(if (null? lst) result (length (cdr lst) (+ result 1))))  ; Return length of list
(define (mean lst)(if (null? lst) 0 (exact->inexact (/ (sum lst 0) (length lst 0)))))  ; Compute mean value for items in list
(define (sum lst result)(if (< (length lst 0) 1) result (sum (cdr lst) (+ result (car lst)))))  ; Tail recursion to compute sum of a list
(define (x_list lst result)(if (null? lst) result (x_list (cdr lst) (cons (car (car lst)) result))))  ; Return list of all X elements in list of pairs
(define (y_list lst result)(if (null? lst) result (y_list (cdr lst) (cons (cadr (car lst)) result))))  ; Return list of all Y elements in list of pairs
#|----------------------------------------------------------------------------|#


#| DEV |#

(define (m_lower lst result)
  (if (< (length lst 0) 1)
      result
      (m_lower (cdr lst) (string->number(real->decimal-string(+ result (square (- (car lst) (xmean)))) 5))))
  )

(define (xmean) (mean (x_list pts1 null)))  ; Is this a variable? Should ask the professor (TODO)
(define (ymean) (mean (y_list pts1 null)))  ; Is this a variable? Should ask the professor (TODO)

#| SANDBOX |#




#| TRACES |#



#| TESTS |#
(mean (x_list pts1 null))
(mean (y_list pts1 null))

(m_lower (x_list pts1 null) 0)