#lang racket

(provide (all-defined-out))
(require racket/trace)
(require "PTS.rkt")

#| PROD |#

#|-------------------------------Helper Functions-----------------------------|#
(define (square x) (* x x ))  ; return x^2
(define (length lst return)(if (null? lst) return (length (cdr lst) (+ return 1))))  ; Return length of list
(define (mean lst)(if (null? lst) 0 (exact->inexact (/ (sum lst 0) (length lst 0)))))  ; Compute mean value for items in list
(define (sum lst return)(if (< (length lst 0) 1) return (sum (cdr lst) (+ return (car lst)))))  ; Tail recursion to compute sum of a list
(define (x_list lst return)(if (null? lst) return (x_list (cdr lst) (cons (car (car lst)) return))))  ; Return list of all X elements in list of pairs
(define (y_list lst return)(if (null? lst) return (y_list (cdr lst) (cons (cadr (car lst)) return))))  ; Return list of all Y elements in list of pairs
#|----------------------------------------------------------------------------|#

#|-------------------------------Calculate M----------------------------------|#
(define (m x_list y_list)(/ (m_upper x_list y_list 0) (m_lower x_list 0)))  ; Calculate M

(define (m_upper x_list y_list return)  ; Calculate numerator for M
  (if (< (length x_list 0) 1)
      return
      (m_upper (cdr x_list) (cdr y_list) (string->number(
                                                         real->decimal-string(+
                                                                              return
                                                                              (*
                                                                                 (- (car x_list) (xmean))
                                                                                 (- (car y_list) (ymean))))
                                                                             5))))
  )

(define (m_lower lst return)  ; Calculate denominator for M
  (if (< (length lst 0) 1)
      return
      (m_lower (cdr lst) (string->number(
                                         real->decimal-string(+
                                                              return
                                                              (square(-
                                                                      (car lst)
                                                                      (xmean))))
                                                             5))))
  )

#|----------------------------------------------------------------------------|#

#| DEV |#


#| SANDBOX |#



#| TODO (ASK TEACHER)|#
(define (xmean) (mean (x_list pts1 null)))  ; Is this a variable? Should ask the professor (TODO)
(define (ymean) (mean (y_list pts1 null)))  ; Is this a variable? Should ask the professor (TODO)


#| TRACES |#


#| TESTS |#
(m (x_list pts1 null) (y_list pts1 null))
