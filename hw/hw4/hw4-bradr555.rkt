#lang racket

(require racket/trace)
;(require "program.rkt")


(provide (all-defined-out))

#|------------------------------- Helpers ------------------------------------|#
(define (first lst)  (car lst))
(define (second lst) (cadr lst))
(define (third lst)  (second (cdr lst)))
#|----------------------------------------------------------------------------|#


(define (synchk program)
  (expr? program `())
  )

(define (expr? program function)
  (or (number? program)
      (symbol? program)
      (op_expr? program function)
      (func_expr? program function)
      (apply_func? program function))
  )

#| Operation logic |#
(define (arith_op? el)
  (or (equal? el `+)
      (equal? el `-)
      (equal? el `*)
      (equal? el `/)))


(define (op_expr? expr function)
  (or (arith_expr? expr function)
      (cond_expr?  expr function)
      (var_expr?   expr function))
  )

(define (arith_expr? expr function)
  (and (equal?    (length expr) 3)
       (arith_op? (first expr))
       (expr?     (second expr) function)
       (expr?     (third expr)  function)
       )
  )

(define (cond_expr? expr function)
  (and (equal? (length expr) 3)
       (ccond? (first expr)  function)
       (expr?  (second expr) function)
       (expr?  (third expr)  function)
       )
  )


(define (ccond? expr function)
  (if (not (list? expr))
      false
      (if (bcond? expr function)
          true
          (if (equal? (length expr) 2)
              (and (equal? (first expr) `not)
                   (ccond? (second expr) function))
              (if (not (equal? (length expr) 3))
                  false
                  (or (and (equal? (first expr) `or)
                           (ccond? (second expr) function)
                           (ccond? (third expr)  function))
                      (and (equal? (first expr) `and)
                           (ccond? (second expr) function)
                           (ccond? (third expr)  function))
                      )
                  )
              )
          )
      )
  )

(define (bcond? expr function)
  (if (not (equal? (length expr) 3))
      false
      (if (not (or (equal? (first expr) `gt)
                   (equal? (first expr) `lt)
                   (equal? (first expr) `eq)))
          false
          (and (expr? (second expr) function)
               (expr? (third expr)  function))
          )
      )
  )

(define (func_expr? expr function)
  (and (equal? (length expr) 3)
       (equal? (first expr) 'fun) ; may need to change 'fun to 'program
       (func_assign? (second expr) (append function (first (second expr))))
       (expr? (third expr) (append function (first (second expr))))
       )
  )

(define (func_assign? expr function)
  (and (equal? (length expr) 2)
       (equal? (length (first expr)) 2)
       (symbol? (second expr))
       (formal_params? (second (first expr)))
       (expr? (second expr) function)
       )
  )

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

(define (apply_func? expr function)
  (and (equal? (length expr) 2)
       (equal? (length (second expr)) 2)
       (equal? (first expr) `apply) ; may need to rename this
       (symbol? (first (second expr)))
       (args? (second (second expr)) function)
       (match? (second expr) function)
       )
  )


(define (args? expr function)
  (cond
    [ (not (list? expr))              false ]
    [ (null? expr)                    true  ]
    [ (arg_list? expr function)       true  ] 
    [ (not (arg_list? expr function)) false ])
  )

(define (match? expr function)
  (if (null? function)
      false
      (if (and (equal? (first expr) (first function))
               (equal? (length (second expr)) (length (second function))))
          true
          (match? expr (cddr function))
          )
      )
  )

(define (arg_list? expr function)
  (if (equal? (length expr) 1)
      (expr? (first expr) function)
      (and (expr? (first expr) function)
           (arg_list? (cdr expr) function)))
  )

(define (var_expr? expr function)
  (or (and (equal? (length expr) 3)
           (equal? (first expr) `var)
           (var_assigned? (second expr) function)
           (expr? (third expr) function))
      (and (equal? (length expr) 2)
           (var_assigned? expr function)))
  )

(define (var_assigned? expr function)
  (if (null? expr)
      false
      (var_assigned_seq? expr function))
  )

(define (var_assigned_seq? expr function)
  (if (not (list? expr))
      false
      (if (null? expr)
          true
          (if (not (list? (first expr)))
              (and (equal? (length expr) 2)
                   (not (equal? (car expr) `apply))
                   (symbol? (first expr))
                   (expr? (second expr) function))
              (and (equal? (length (first expr)) 2)
                   (symbol? (first (first expr)))
                   (expr? (second (first expr)) function)
                   (var_assigned_seq? (cdr expr) function))
              )
          )
      )
  )


#|------------------------------- Traces n Tests------------------------------|#



; TEST for SYNCHK
(require "synchk_test.rkt")

(print false)(synchk program1)
(print false)(synchk program2)
(print false)(synchk program3)

(display "\nDecl Test\n")
(print false)(synchk program4)
(print false)(synchk program5)
(print true)(synchk program6)

(display "\nAssign Test\n")
(print true)(synchk program7)
(print false)(synchk program8)
(print false)(synchk program9)
(print false)(synchk program10)
(print false)(synchk program11)
(print false)(synchk program12)
(print true)(synchk program13)
(print false)(synchk program14)
(print false)(synchk program15)
(print true)(synchk program16)
(print false)(synchk program17)

(display "\nIF Test\n")
(print false)(synchk program18)
(print true)(synchk program19)
(print true)(synchk program20)
(print true)(synchk program21)
(print false)(synchk program22)
(print false)(synchk program23)
(print false)(synchk program24)
(print true)(synchk program25)

(display "\nWhile Test\n")
(print false)(synchk program26)
(print true)(synchk program27)
(print true)(synchk program28)
(print true)(synchk program29)
(print false)(synchk program30)
(print false)(synchk program31)
(print false)(synchk program32)
(print false)(synchk program33)


#|----------------------------------------------------------------------------|#

