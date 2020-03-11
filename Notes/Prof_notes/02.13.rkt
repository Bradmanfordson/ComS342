#lang racket
(require racket/trace)

;; Expr -> number | (+ Expr Expr) | ...

;; (synchk '(+ 1 2))
;; think about some representative test cases
;; to test whether synchk produces the expected results
;; (for both true and false outputs)
(define (synchk expr)
  (if (number? expr)
      true
      (if (list? expr)
          (if (equal? (length expr) 3)
              (if (or (equal? (car expr) '+)
                      (equal? (car expr) '-))
                  (and (synchk (cadr expr))
                       (synchk (cadr (cdr expr))))
                  false)
              false)
          false)))

;; translates prefix to infix notation
;; (trans '(+ 1 2))
;; review the similarity in structure between trans
;; and synchk
(define (trans expr)
  (if (number? expr)
      expr
      (if (list? expr)
          (if (equal? (length expr) 3)
              (if (or (equal? (car expr) '+)
                      (equal? (car expr) '-))
                  (cons (trans (second expr))
                        (cons (first expr)
                              (list (trans (third expr)))))
                  false)
              false)
          false)))

;; I prefer to use semantically meaningful names for functions
;; to avoid making mistakes in nesting car, cdr, cadr, etc. 
(define (first lst) (car lst))
(define (second lst) (cadr lst))
(define (third lst) (cadr (cdr lst)))

;; this is synchk1 using cond syntactic structure. You can
;; use cond in your assignments
(define (synchk1 expr)
  (cond
    [ (number? expr)   true ]
    [ (and (list? expr) (equal? (length expr) 3)) (if (or (equal? (first expr) '+)
                                                          (equal? (first expr) '-))
                                                      (and (synchk1 (second expr))
                                                           (synchk1 (third expr)))
                                                      false) ]
    [ else  false ]))
                                                      

;; Write a translator from infix to prefix
;; check whether it is working by first
;; translating prefix to infix (using the above function)
;; and then use your new translator to get the prefix back
;; and examine whether the final output is identical to
;; the original input.