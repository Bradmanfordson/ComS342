#lang racket

;(require "tests.rkt")
(provide (all-defined-out))

(require racket/trace)


#| TEMP CODE - DELETE LATER |#
(define requests
  `((0 8)  (8 11)
    (1 3)  (4 13) (5 6)
    (3 10) (7 9)  (10 12)))
(define requests2
  `((0 1)  (1 2)
    (2 3)  (6 13) (5 6)
    (3 10) (7 9) ))


#| HELPERS |#
(define (pair_sort a b) (<= (car a) (car b)))

(define (get_first lst) (car (sort lst pair_sort)))
(define (get_last lst)  (cdr (sort lst pair_sort)))

(define (length lst return)(if (null? lst) return (length (cdr lst) (+ return 1))))  ; Return length of list

(define (update_env val env)
  (if (null? env)
      (cons (list (first val) (second val)) env)
      (cons (list (first val) (second val)) env)
              ))

#| Get Request List |#
(define (getreqlst lst symbol)
  (cond
    [(equal? symbol `st) (do_st (get_first lst) (get_last lst) ((list (get_first lst) )))]
    [(equal? symbol `ft) (sort lst pair_sort)]
    [(equal? symbol `sh) (sort lst pair_sort)]
    ))

(define (do_st first last return); lst)
  (if (equal? last  `())
          return 
          (if (or (< (car (cdr first)) (car (car last)))
                  (equal? (car (cdr first)) (car (car last))))
              (do_st (car last) (cdr last) (list return (car last)))
              (do_st first (cdr last) return) 
              )))



(define (do_ft lst) true)
(define (do_sh lst) true)

;(define (st_sort first last)
;  (if (< (cdr first) (car (car last)))
;      (list first last)
;      (st_sort 
;(define (tmp lst f_el)
;  (if (< f_el (car lst))))


#| TRACES |#
(trace do_st)


#| TESTS |#
(get_first (sort requests pair_sort))
(get_last  (sort requests pair_sort))
(getreqlst requests `st)
(getreqlst requests2 `st)
(getreqlst requests `ft)
(getreqlst requests `sh)


      

