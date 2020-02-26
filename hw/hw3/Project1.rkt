#lang racket

(provide (all-defined-out))
(require racket/trace)
(require "PTS.rkt")

#|-------------------------------Helper Functions-----------------------------|#
(define (length lst result)  ; Return the length of a list
  (if (null? lst)
      result ; base case
      (length (cdr lst) (+ result 1)))
  )  ; FINISHED

(define (x_list lst result)  ; Return list of all X elements in list of pairs
  (if (null? lst)
      result
      (x_list (cdr lst) (cons (car (car lst)) result)))
  )  ; FINISHED

(define (y_list lst result)  ; Return list of all Y elements in list of pairs
  (if (null? lst)
      result
      (y_list (cdr lst) (cons (cadr (car lst)) result)))
  )  ; FINISHED

(define (sum lst result)  ; Tail recursion to compute sum of a list
  (if (< (length lst 0) 1)
      result
      (sum (cdr lst) (+ result (car lst))))
  )  ; FINISHED

(define (mean lst)  ; Compute mean value for items in list
  (if (null? lst)
      0
      (exact->inexact (/ (sum lst 0) (length lst 0))))
  )  ; FINISHED
#|----------------------------------------------------------------------------|#

#| TEST BED |#


#| TRACES |#

#| TESTS |#
(mean (x_list pts1 null))
(mean (y_list pts1 null))
