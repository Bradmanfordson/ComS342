#lang racket
(require racket/trace)

#| SEMANTICS CHECKER |#
;; expr --> number | ( + expr expr ) | ( - expr expr)
(define (sem expr)
  (cond
    ; is this a number?
    [ (number? expr) expr ]

    ; is this a + or a -?
    [ (arithmetic_operator? (car expr)) (arithmetic_semantics expr)]

    ; is this an if-then-else statement?
    [ (conditional_operator? (car (car expr)) (if (conditional_semantics (car expr)) ;if
                                                  (sem (cadr expr)) ;then
                                                  (sem (cadr (cdr expr)))))] ;else
    )
  )


#| CONDITIONAL STUFF |#
(define (conditional_operator? element)
  (or (equal? element 'or)
      (equal? element 'and)
      (equal? element 'not)
      (equal? element 'gt)
      (equal? element 'lt)
      (equal? element 'eq)
      )
  )

(define (conditional_semantics expr)
  (cond
    [ (equal? (car expr) 'gt) (> (sem (cadr expr)) (sem (cadr (cdr expr))))]
    [ (equal? (car expr) 'lt) (< (sem (cadr expr)) (sem (cadr (cdr expr))))]
    [ (equal? (car expr) 'or) (or (conditional_semantics (cadr expr)) (sem (cadr (cdr expr))))]
    [ (equal? (car expr) 'and) (and (conditional_semantics (cadr expr)) (sem (cadr (cdr expr))))]
    [ (equal? (car expr) 'not) (not (conditional_semantics (cadr expr)) (sem (cadr (cdr expr))))]
    ;[ (equal? (car expr) 'eq) (equals? (conditional_semantics (cadr expr)) (sem (cadr (cdr expr))))]
    )
  )

#| ARITHMETIC STUFF |#

;; if element is '+' or '-': return True
(define (arithmetic_operator? element)
  (or (equal? element `+)
      (equal? element `-)
      )
  )


(define (arithmetic_semantics expr)
  (cond
    [ (equal? (car expr) `+) (+ (sem (cadr expr))
                                (sem (cadr (cdr expr))))]
    [ (equal? (car expr) `-) (- (sem (cadr expr))
                                (sem (cadr (cdr expr))))]
    )
  )


#| TRACES |#
(trace sem)
(trace arithmetic_operator?)
(trace arithmetic_semantics)