#lang racket

(require racket/trace)
;(require "program.rkt")


(provide (all-defined-out))

#|------------------------------- Helpers ------------------------------------|#
(define (first lst)  (car lst))
(define (second lst) (cadr lst))
(define (third lst)  (second (cdr lst)))
#|----------------------------------------------------------------------------|#

#|------------------------------- Synchk -------------------------------------|#
(define (synchk program)
  (if (and (list? program)         ; Is the program a list?
           (equal? (length program) 1)) ; Does the program list have more than 1 element?
      #| THEN |# (expr? (car program))
      #| ELSE |# (if (and (list? (car program))     ; Is the first element in the program list also a list?
                          (list? (cdr program)))    ; Is the rest of the program a list?
                    #| THEN |# (and (expr? (car program))
                                    (synchk (cdr program)))
                    #| ELSE |# false )))


(define (expr? expr)
  (or (number?  expr)
      (symbol?  expr)
      (decl?    expr)
      (assign?  expr)
      (op_expr? expr)
      (if?      expr)
      (while?   expr)))

#| Decl |#
(define (decl? expr)
  (and (equal?  (length expr)     2)
       (equal?  (first  expr) `decl)
       (symbol? (second expr)      )))

#| Assign |#
(define (assign? expr)
  (and (equal?      (length expr)       3)
       (equal?      (first  expr) `assign)
       (symbol?     (second expr)        )
       (arith_expr? (third  expr)        )))

#|OP EXPR|#
(define (op_expr? expr)
  (or (arith_expr? expr)
      (cond_expr?  expr)))

#| IF |#
(define (if? expr)
  (and (equal? (length expr)   3)
       (equal? (first  expr) `if)
       (ccond? (second expr)    )
       (synchk (third  expr)    )))

#| WHILE |# 
(define (while? expr)
  (and (equal? (length expr)      3)
       (equal? (first  expr) `while)
       (ccond? (second expr)       )
       (synchk (third  expr)       )))

#| Helpers for Symchk functions|#
(define (arith_op? el)
  (or (equal? el `+)
      (equal? el `-)
      (equal? el `*)
      (equal? el `/)))

(define (arith_expr? expr )
  (or (number? expr)
      (symbol? expr)
      (and (equal?(length expr) 3)
           (arith_op?   (first  expr))
           (arith_expr? (second expr))
           (arith_expr? (third  expr)))))

(define (cond_expr? expr)
  (or (bcond? expr)
      (and (equal? (length expr) 3)
           (ccond? (first  expr)  )
           (expr?  (second expr)  )
           (expr?  (third  expr)  ))))

(define (ccond? expr )
  (if (not (list? expr))
      false
      (if (bcond? expr )
          true
          (if (equal? (length expr) 2)
              (and (equal? (first  expr) `not)
                   (ccond? (second expr)     ))
              (if (not (equal? (length expr) 3))
                  false
                  (or (and (equal? (first  expr)  `or)
                           (ccond? (second expr)     )
                           (ccond? (third  expr)     ))
                      (and (equal? (first  expr) `and)
                           (ccond? (second expr)     )
                           (ccond? (third  expr)     ))))))))
 
(define (bcond? expr )
  (if (not (equal? (length expr) 3))
      false
      (if (not (or (equal? (first expr) `gt)
                   (equal? (first expr) `lt)
                   (equal? (first expr) `eq)))
          false
          (and (expr? (second expr) )
               (expr? (third  expr) )))))

#|------------------------------- End of Synchk ------------------------------|#




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
(print 33)(print true)(synchk program33)


(display "\n ---------- SEM TESTS ---------- \n")
(display "\n34: ")(synchk program34)
(display "\n35: ")(synchk program35)
(display "\n36: ")(display "'((x 0) (y 1)) => '((x 6) (y 1)) \n\t")(synchk program36)
(display "\n37: ")(display "'() => '((y 7) (x 8)) \n\t")(synchk program37)
(display "\n38: ")(display "'() => '((y 6) (x 7)) \n\t")(synchk program38)
(display "\n39: ")(display "'() => '((y 7) (x 6)) \n\t")(synchk program39)
(display "\n40: ")(display "'() => '((y 6) (x 8)) \n\t")(synchk program40)
(display "\n41: ")(display "'() => '((y 7) (x 6)) \n\t")(synchk program41)
(display "\n42: ")(display "'() => '((y 7) (x 8)) \n\t")(synchk program42)
(display "\n43: ")(display "'() => '((y 7) (x 8)) \n\t")(synchk program43)
(display "\n44: ")(display "'() => '((y 6) (x 6)) \n\t")(synchk program44)
(display "\n45: ")(display "'() => '((z 5) (y 7) (x 8)) \n\t")(synchk program45)
(display "\n46: ")(display "'() => '((z 5) (y 7) (x 6)) \n\t")(synchk program46)
(display "\n47: ")(display "'() => '((z 7) (y 5) (x 6)) \n\t")(synchk program47)
(display "\n49: ")(display "'() => '((y 7) (x 6)) \n\t")(synchk program49)
(display "\n50: ")(display "'() => '((y 6) (x 8)) \n\t")(synchk program50)
(display "\n51: ")(display "'() => '((y 2) (x 4)) \n\t")(synchk program51)
(display "\n52: ")(display "'() => '((y 2) (x 0)) \n\t")(synchk program52)
(display "\n53: ")(display "'() => '((y 2) (x 6)) \n\t")(synchk program53)
(display "\n54: ")(display "'() => '((y 2) (x 1)) \n\t")(synchk program54)
(display "\n55: ")(display "'() => '((y 3) (x 10)) \n\t")(synchk program55)
(display "\n56: ")(display "'() => '((y 256) (x 0)) \n\t")(synchk program56)
(display "\n57: ")(display "'() => '((y 4) (x 4)) \n\t")(synchk program57)
(display "\n58: ")(display "'() => '((y 4) (x 4)) \n\t")(synchk program58)

#|--------------------------------- FIN -------------------------------------|#
