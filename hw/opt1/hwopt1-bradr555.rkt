#lang racket
(require "tests.rkt")
(provide (all-defined-out))

#| HELPERS |#

(define (length lst return)
  (if (null? lst)
      return
      (length (cdr lst) (+ return 1))))  ; Return length of list

(define (add2end el lst)
  (if (null? lst)
      (cons el `())
      (cons (car lst) (add2end el (cdr lst)))))

(define (st_sort a b) (<= (car a) (car b)))
(define (st_first lst) (car (sort lst st_sort)))
(define (st_last lst)  (cdr (sort lst st_sort)))

(define (ft_sort a b) (<= (cadr a) (cadr b)))
(define (ft_first lst) (car (sort lst ft_sort)))
(define (ft_last lst)  (cdr (sort lst ft_sort)))

#| Get Request List |#

(define (getreqlst lst symbol)
  (cond
    [(equal? symbol `st) (do_st (st_first lst) (st_last lst) (list (st_first lst)))]
    [(equal? symbol `ft) (do_ft (ft_first lst) (ft_last lst) (list (ft_first lst)))]
    [(equal? symbol `sh) (do_st (ft_first lst) (ft_last lst) (list (ft_first lst)))]
    ))

(define (do_st first last return); lst)
  (if (equal? last  `())
          return 
          (if (or (< (car (cdr first)) (car (car last)))
                  (equal? (car (cdr first)) (car (car last))))
              (do_st (car last) (cdr last) (add2end (car last) return))
              (do_st first (cdr last) return) 
              )))

(define (do_ft first last return); lst)
  (if (equal? last  `())
          return 
          (if (or (< (cadr first) (cadr (car last)))
                  (equal? (cadr first) (cadr (car last))))
              (do_ft (car last) (cdr last) (add2end (car last) return))
              (do_ft first (cdr last) return) 
              )))

  
#| LSEM |#
 
(define (lsem lst) lst)

