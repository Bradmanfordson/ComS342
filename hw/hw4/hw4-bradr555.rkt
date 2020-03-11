#lang racket

(require racket/trace)
;(require "program.rkt")


(provide (all-defined-out))

#|------------------------------- Helpers ------------------------------------|#
(define (first lst)  (car lst))
(define (second lst) (cadr lst))
(define (third lst)  (second (cdr lst)))
#|----------------------------------------------------------------------------|#


#|------------------------------- Finished -----------------------------------|#
(define (arithop? el)
  (or (equal? el `+)
      (equal? el `-)
      (equal? el `*)
      (equal? el `/)))

(define (cond? el)
    (or (equal? el `and)
        (equal? el `or)
        (equal? el `not)))

(define (bcond? el)
  (or (equal? el `gt)
      (equal? el `lt)
      (equal? el `eq)))

#|----------------------------------------------------------------------------|#





#|------------------------------- NOT WORKING -----------------------------------|#

(define (arithmetic_semantics expr env)
  (cond
    [ (equal? (car expr) `+) (+ (sem (cadr expr) env)
                                (sem (cadr (cdr expr)) env)) ]
    
    [ (equal? (car expr) `-) (- (sem (cadr expr) env)
                                (sem (cadr (cdr expr)) env)) ]
    
    [ (equal? (car expr) `*) (* (sem (cadr expr) env)
                                (sem (cadr (cdr expr)) env)) ]
    
    [ (equal? (car expr) `/) (/ (sem (cadr expr) env)
                                (sem (cadr (cdr expr)) env)) ]
    )
  )


(define (conditional_semantics expr env)
  (cond
    [ (equal? (car expr) `or)  (or (conditional_semantics (cadr expr) env)         ; Pull x in (or x y)
                                   (conditional_semantics (cadr (cdr expr)) env))] ; Pull y in (or x y)
    
    [ (equal? (car expr) `and) (and (conditional_semantics (cadr expr) env)         ; Pull x in (or x y)
                                    (conditional_semantics (cadr (cdr expr)) env))] ; Pull y in (or x y)
    
    [ (equal? (car expr) `not) (not (cadr expr)) ] ; not of x in (not x)

    [ (equal? (car expr) `gt)  (> (sem (cadr expr) env)
                                  (sem (cadr (cdr expr)) env)) ]

    [ (equal? (car expr) `lt)  (< (sem (cadr expr) env)
                                  (sem (cadr (cdr expr)) env)) ]

    [ (equal? (car expr) `eq)  (equal? (sem (cadr expr) env)
                                       (sem (cadr (cdr expr)) env)) ]
    )
  )

(define (sem expr env)
  (cond
    [ (number? expr)                            expr ]
    
    ;[ (symbol? expr)                            (findvalue expr env)] ;? what do?
    
    [ (arithop? (car (car expr)))         (arithmetic_semantics expr env)]
    
    ;[ (equal? (car expr) `var)                  (sem (caadr expr) (conditional_semantics (cadr expr) env))]
    
    ;[ (conditional_operator? (car expr))        (if (conditional_semantics (car expr) env)
                                                    ;(sem (cadr  expr) env)
                                                    ;(sem (caadr expr) env))]
    )
  )
#|----------------------------------------------------------------------------|#







#|------------------------------- Synchk (not working) -------------------------------------|#
(define (synchk expr)
  (cond
    [ (or (symbol? expr)
          (number? expr)
          (list? expr))
      true ]
    
    [ (and (list? expr) (equal? (length expr) 2)) (if (or
                                                       (equal? (first expr) `decl)
                                                       (equal? (first expr) `not))
                                                      (synchk (second expr))
                                                      false) ]
    
    [ (and (list? expr) (equal? (length expr) 3)) (if (or (equal? (first expr) `+)
                                                          (equal? (first expr) `-)
                                                          (equal? (first expr) `*)
                                                          (equal? (first expr) `/)
                                                          (equal? (first expr) `or)
                                                          (equal? (first expr) `and)
                                                          (equal? (first expr) `not)
                                                          (equal? (first expr) `gt)
                                                          (equal? (first expr) `lt)
                                                          (equal? (first expr) `eq)
                                                          (equal? (first expr) `assign); Prolly have to do more
                                                          (equal? (first expr) `if)    ; Prolly have to do more
                                                          (equal? (first expr) `while) ; Prolly have to do more 
                                                          )
                                                      (and (synchk (second expr))
                                                           (synchk (third expr)))
                                                      false) ]
    [ else false ]
    )
  )

   
#|----------------------------------------------------------------------------|#



#|------------------------------- Traces n Tests------------------------------|#



; TEST for SYNCHK
(require "synchk_test.rkt")
(synchk program1)
(synchk program2)
(synchk program3)
(synchk program4)
(synchk program5)
(synchk program6)
(synchk program7)
(synchk program8)
(synchk program9)
(synchk program10)
(synchk program11)
(synchk program12)
(synchk program13)
(synchk program14)
(synchk program15)
(synchk program16)
(synchk program17)
(synchk program18)
(synchk program19)
(synchk program20)
(synchk program21)
(synchk program22)
(synchk program23)
(synchk program24)
(synchk program25)
(synchk program26)
(synchk program27)
(synchk program28)
(synchk program29)
(synchk program30)
(synchk program31)
(synchk program32)
(synchk program33)


#|----------------------------------------------------------------------------|#

