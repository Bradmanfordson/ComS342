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

#|-------------------------------Calculate M and C----------------------------|#
(define (m x_list y_list xmean ymean)(/ (m_upper x_list y_list xmean ymean 0) (m_lower x_list xmean 0)))  ; Calculate M

(define (m_upper x_list y_list xmean ymean return)  ; Calculate numerator for M
  (if (< (length x_list 0) 1)
      return
      (m_upper (cdr x_list) (cdr y_list) xmean ymean (string->number(
                                                         real->decimal-string(+
                                                                              return
                                                                              (*
                                                                                 (- (car x_list) xmean)
                                                                                 (- (car y_list) ymean)))
                                                                             5))))
  )

(define (m_lower lst xmean return)  ; Calculate denominator for M
  (if (< (length lst 0) 1)
      return
      (m_lower (cdr lst) xmean (string->number(
                                         real->decimal-string(+
                                                              return
                                                              (square(-
                                                                      (car lst)
                                                                      xmean)))
                                                             5))))
  )

(define (c lst xmean ymean)  ; return --> mean(y) - m * mean(x)
  (-
   ymean
   (*
    (m (x_list lst null) (y_list lst null) xmean ymean)
    xmean))
  )
#|----------------------------------------------------------------------------|#

#|-------------------------------compute_mc----------------------------------|#
(define (compute_mc lst)
  (list (m (x_list lst null) (y_list lst null) (mean (x_list lst null)) (mean (y_list lst null)))
        (c lst (mean (x_list lst null)) (mean (y_list lst null))))
  
  )
#|----------------------------------------------------------------------------|#


#|-------------------------------compute_e----------------------------------|#
(define (e lst)
  (*
   (/ 1 (length lst 0)) ; 1/n
   (sum_e
    lst ; LIST
    (x_list lst null) ; X_LIST
    (y_list lst null) ; Y_LIST
    (mean (x_list lst null)) ; XMEAN
    (mean (y_list lst null)) ;YMEAN
    (m (x_list lst null) (y_list lst null) (mean (x_list lst null)) (mean (y_list lst null))) ;M
    (c lst (mean (x_list lst null)) (mean (y_list lst null)));C
    0) ;RETURN
   )
  )

(define (sum_e lst x_list y_list xmean ymean m c return)
  (if (< (length x_list 0) 1)
      return
      (sum_e
       lst
       (cdr x_list)
       (cdr y_list)
       xmean
       ymean
       m
       c
       (+
        return
        (square
         (-
          (+
           (*
            m
            (car x_list))
           c)
          (car y_list)))))
      )
)
      
#|----------------------------------------------------------------------------|#



#| TRACES |#
(trace e)
(trace sum_e)

 
#| TESTS |#
(compute_mc pts1)
(compute_mc pts2)
(compute_mc pts3)

(m (x_list pts1 null) (y_list pts1 null) (mean (x_list pts1 null)) (mean (y_list pts1 null)))
(e pts1)
