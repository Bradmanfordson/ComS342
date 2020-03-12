#lang racket

(require racket/trace)
;(require "program.rkt")


(provide (all-defined-out))

#|------------------------------- Helpers ------------------------------------|#
(define (first lst)  (car lst))
(define (second lst) (cadr lst))
(define (third lst)  (second (cdr lst)))
#|----------------------------------------------------------------------------|#
(define (test program) true)


(define (synchk program)
  (if (and (list? program)         ; Is the program a list?
           (equal? (length program) 1)) ; Does the program list have more than 1 element?
      #| THEN |# (expr? (car program))
      #| ELSE |# (if (and (list? (car program))     ; Is the first element in the program list also a list?
                          (list? (cdr program)))    ; Is the rest of the program a list?
                    #| THEN |# (and (expr? (car program))
                                    (test (cdr program)))
                    #| ELSE |# false)

                 )
  )


(define (expr? expr)
  (or (number?  expr)
      (symbol?  expr)
      (decl?    expr)
      (assign?  expr)
      (op_expr? expr)
      (if?      expr)
      (while?   expr)
      )
  )

(define (if? expr)
  (and (equal? (length expr)   3)
       (equal? (first  expr) `if)
       (ccond? (second expr)    )
       (synchk (third  expr)    ))
  )

(define (while? expr)
  (and (equal? (length expr)      3)
       (equal? (first  expr) `while)
       (ccond? (second expr)       )
       (synchk (third  expr)       ))
  )

#| Decl |#
(define (decl? expr)
  (and (equal?  (length expr)     2)
       (equal?  (first  expr) `decl)
       (symbol? (second expr)      ))
  )

#| Assign |#
(define (assign? expr)
  (and (equal?      (length expr)       3)
       (equal?      (first  expr) `assign)
       (symbol?     (second expr)        )
       (arith_expr? (third  expr)        ))
  )

#| Op |#
(define (arith_op? el)
  (or (equal? el `+)
      (equal? el `-)
      (equal? el `*)
      (equal? el `/))
  )


(define (op_expr? expr )
  (or (arith_expr? expr )
      (cond_expr?  expr )
      )
  )

#| ArithExpr |#
(define (arith_expr? expr )
  (or (number? expr)
      (symbol? expr)
      (and (equal?(length expr) 3)
       (arith_op? (first expr))
       (arith_expr?     (second expr))
       (arith_expr?     (third expr))
       ))
  )

#| CondExpr |# 
(define (cond_expr? expr )
  (and (equal? (length expr) 3)
       (ccond? (first expr )  )
       (expr?  (second expr)  )
       (expr?  (third expr )  )
       )
  )


(define (ccond? expr )
  (if (not (list? expr))
      false
      (if (bcond? expr )
          true
          (if (equal? (length expr) 2)
              (and (equal? (first expr) `not)
                   (ccond? (second expr) ))
              (if (not (equal? (length expr) 3))
                  false
                  (or (and (equal? (first expr) `or)
                           (ccond? (second expr) )
                           (ccond? (third expr)  ))
                      (and (equal? (first expr) `and)
                           (ccond? (second expr) )
                           (ccond? (third expr)  ))
                      (and (equal? (first expr) `not)
                           (ccond? (second expr) )
                           (ccond? (third expr)  ))
                      )
                  )
              )
          )
      )
  )

#| BoolCond |# 
(define (bcond? expr )
  (if (not (equal? (length expr) 3))
      false
      (if (not (or (equal? (first expr) `gt)
                   (equal? (first expr) `lt)
                   (equal? (first expr) `eq)))
          false
          (and (expr? (second expr) )
               (expr? (third expr)  ))
          )
      )
  )


#|  stuff (dont think this is needed) |#
(define (func_expr? expr )
  (and (equal? (length expr) 3)
       (equal? (first expr) 'fun) ; may need to change 'fun to 'program
       (func_assign? (second expr) (append  (first (second expr))))
       (expr? (third expr) (append  (first (second expr))))
       )
  )

(define (func_assign? expr )
  (and (equal? (length expr) 2)
       (equal? (length (first expr)) 2)
       (symbol? (second expr))
       (formal_params? (second (first expr)))
       (expr? (second expr) )
       )
  )

#| Formal parameters (Dont think this is needed) |#
(define (formal_params? expr)
  (cond
    [ (not (list? expr))              false ]
    [ (null? expr)                    true  ]
    [ (formal_param_list? expr)       true  ]
    [ (not (formal_param_list? expr)) false ]
    )
  )

(define (formal_param_list? expr)
  (if (equal? (length expr) 1)
      (symbol? (first expr))
      (and (symbol? (first expr)) (formal_param_list? (cdr expr)))
      )
  )

(define (apply_func? expr )
  (and (equal? (length expr) 2)
       (equal? (length (second expr)) 2)
       (equal? (first expr) `apply) ; may need to rename this
       (symbol? (first (second expr)))
       (args? (second (second expr)) )
       (match? (second expr) )
       )
  )

#|  args (prolly not needed) |#
(define (args? expr )
  (cond
    [ (not (list? expr))              false ]
    [ (null? expr)                    true  ]
    [ (arg_list? expr )       true  ] 
    [ (not (arg_list? expr )) false ])
  )

(define (match? expr )
  (if (null? )
      false
      (if (and (equal? (first expr) (first ))
               (equal? (length (second expr)) (length (second ))))
          true
          (match? expr (cddr ))
          )
      )
  )

(define (arg_list? expr )
  (if (equal? (length expr) 1)
      (expr? (first expr) )
      (and (expr? (first expr) )
           (arg_list? (cdr expr) )))
  )

#| Var |#
(define (var_expr? expr )
  (or (and (equal? (length expr) 3)
           (equal? (first expr) `var)
           (var_assigned? (second expr) )
           (expr? (third expr) ))
      (and (equal? (length expr) 2)
           (var_assigned? expr )))
  )

(define (var_assigned? expr )
  (if (null? expr)
      false
      (var_assigned_seq? expr ))
  )





#| Sequences |#
(define (var_assigned_seq? expr )
  (if (not (list? expr))
      false
      (if (null? expr)
          true
          (if (not (list? (first expr)))
              (and (equal? (length expr) 2)
                   (not (equal? (car expr) `apply))
                   (symbol? (first expr))
                   (expr? (second expr) ))
              (and (equal? (length (first expr)) 2)
                   (symbol? (first (first expr)))
                   (expr? (second (first expr)) )
                   (var_assigned_seq? (cdr expr) ))
              )
          )
      )
  )

#|------------------------------- Traces n Tests------------------------------|#



; TEST for SYNCHK
(require "synchk_test.rkt")
;(trace synchk)
;(trace test)
;(trace expr?)


(display "\n ---------- SYNCHK TESTS ---------- \n")
(print 1)(print false)(synchk program1)
(print 2)(print false)(synchk program2)
(print 3)(print false)(synchk program3)

(display "\nDecl Test\n")
(print 4)(print false)(synchk program4)
(print 5)(print false)(synchk program5)
(print 6)(print true)(synchk program6)

(display "\nAssign Test\n")
(print 7)(print true)(synchk program7)
(print 8)(print false)(synchk program8)
(print 9)(print false)(synchk program9)
(print 10)(print false)(synchk program10)
(print 11)(print false)(synchk program11)
(print 12)(print false)(synchk program12)
(print 13)(print true)(synchk program13)
(print 14)(print false)(synchk program14)
(print 15)(print false)(synchk program15)
(print 16)(print true)(synchk program16)
(print 17)(print false)(synchk program17)

(display "\nIF Test\n")
(print 18)(print false)(synchk program18)
(print 19)(print true)(synchk program19)
(print 20)(print true)(synchk program20)
(print 21)(print true)(synchk program21)
(print 22)(print false)(synchk program22)
(print 23)(print false)(synchk program23)
(print 24)(print false)(synchk program24)
(print 25)(print true)(synchk program25)

(display "\nWhile Test\n")
(print 26)(print false)(synchk program26)
(print 27)(print true)(synchk program27)
(print 28)(print true)(synchk program28)
(print 29)(print true)(synchk program29)
(print 30)(print false)(synchk program30)
(print 31)(print false)(synchk program31)
(print 32)(print false)(synchk program32)
(print 33)(print false)(synchk program33)


#|----------------------------------------------------------------------------|#

