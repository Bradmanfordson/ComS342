#lang racket
(require racket/trace)
(provide (all-defined-out))
(require "program.rkt")


#| ###################################################################### |#

;; semantics of a program = semantics of rest of the program in the context of semantics of first statement
(define (sem P Env)
  (if (null? (cdr P))
      (semstmt (car P) Env)
      (sem (cdr P) (semstmt (car P) Env))))


(define (semstmt S Env)
  (cond
    ;; declaration 
    [ (equal? (car S) 'decl)   (cons (list (cadr S) 0) Env) ]
    ;; assignment
    [ (equal? (car S) 'assign) (updateValue (cadr S)
                                            (semArith (cadr (cdr S)) Env)
                                            Env) ]
    ;; if
    [ (equal? (car S) 'if)  (removemarker (semIf (semcomplexcond (cadr S) Env) ;; condExpr
                                                 (cadr (cdr S)) ;; sequence of stmts
                                                 (cons (list '$m 0) Env))) ]

    ;; while
    [ (equal? (car S) 'while) (if (semcomplexcond (cadr S) Env) ;; condition is true
                                  (sem (list (list 'if '(eq 1 1) (cadr (cdr S))) ;; sequence of while statements
                                               S) ;; the while statement
                                       Env)
                                  Env) ] ;; return the environment because nothing is changing

    #| Directive item: 2|#
    ;; ADDED for hw5 fundecl (fundecl (fname Paramlst) fDef) 
    [ (equal? (car S) 'fundecl) (cons (list (cadr S) (cadr (cdr S))) Env) ]
  
    ;; ADDED for hw5 dynamic scoping: (call (fname Arglst) X) : not reviewing the last arg for the solution here
    ;; Review directives: findDef finds the appropriate function from the environment
    ;;                    (number of arguments in the call is used)
    ;; semArithList: Evaluates the list of arguments
    ;; semcall: first argument is a list of (ParamList FDef Env-with-marker) and second argument (ArgVals)
    [ (equal? (car S) 'call)  (removemarker (semcall
                                             (findDef (car (cadr S)) (length (cadr (cadr S))) Env Env)
                                             (semArithList (cadr (cadr S)) Env)
                                             )) ]

    ))


#| Directive item: 3 |#
#| ADDED for hw5-dynamic scoping |#

#| Two env? If the search for a function call leads to a variable, we need
   continue the search with the value of the variable, i.e., a function name,
   in the entire environment.
   EnvtoRec: the environment on which we will iterate
   Env: the environment we are keeping in case, we need to do two iterations
   Structure of environment: ((x y) ((f plist) fdef) (z f))
|#
(define (findDef fname nParams EnvtoRec Env )
  (if (equal? (car (car EnvtoRec)) fname)        ;; calling name matches with a variable, search again
      (findDef (cadr (car EnvtoRec)) nParams Env Env) ;; search from the top of environment
      ;; is this a function, does the name of the function match; does the number of params match - then
      
      (if (and (list? (car (car EnvtoRec)))      ;; is a function entry in the env
               (equal? (car (car (car EnvtoRec))) fname) ;; function name matches
               (equal? (length (cadr (car (car EnvtoRec)))) nParams)) ;; number of parameter matches
              (list (cadr (car (car EnvtoRec)))       ;; paramlist
                    (cadr (car EnvtoRec))             ;; Def
                    (cons (list '$m 0) Env))          ;; Dynamic environment with marker Directive item: 5
                                                     
          ;; else continue with the search search in the rest of the environment
           (findDef fname nParams (cdr EnvtoRec) Env))))


#| Directive item: 4 |#
#| ADDED for hw5  create an addition to the enviroment using the parameters-Argvals |#
(define (genEnv Params Args Env)
  (if (null? Params)
      Env
      (cons (list (car Params) (car Args)) (genEnv (cdr Params) (cdr Args) Env))))

(define (semArithList Exprs Env)
  (if (null? Exprs)
      Exprs
      (cons (semArith (car Exprs) Env) (semArithList (cdr Exprs) Env))))

#|
 Rest: for creating the composition. 
 ParamsDef is a list containing (ParameterList Definition)
 Args is a list of argument values
|#
(define (semcall ParamsDefEnv Args)
  (sem (cadr ParamsDefEnv)        ;; semantics of the definition 
       (genEnv (car ParamsDefEnv) ;; genEnv creates the environment by adding mapping of params to argval
               Args
               (cadr (cdr ParamsDefEnv)))))


#| Directive item 1|#
(define (findValue v Env) ;; update to make room for function names being assigned to variables; they do not
                          ;; have values
  (if (null? Env)         ;; couple of lines to add: ADDED for hw5
      v
      (if (equal? v (car (car Env)))
          (cadr (car Env))
          (findValue v (cdr Env)))))

#| The addendum for the function calls ends here |#

(define (semIf condVal SSeq Env)
  (if condVal
      (sem SSeq Env)
      Env))

(define (removemarker Env)
  (if (equal? (car (car Env)) '$m)
      (cdr Env)
      (removemarker (cdr Env))))


(define (updateValue v val Env)
  (if (equal? (car (car Env)) v)
      (cons (list (car (car Env))
                  val)
            (cdr Env))
      (cons (car Env) (updateValue v val (cdr Env)))))


(define (semArith Expr Env)
  (cond
    [ (number? Expr)          Expr ]

    [ (symbol? Expr)          (findValue Expr Env) ]
    
    [ (equal? (car Expr) '+)  (+ (semArith (cadr Expr)  Env)
                                 (semArith (cadr (cdr Expr)) Env)) ]
    [ (equal? (car Expr) '-)  (- (semArith (cadr Expr) Env)
                                 (semArith (cadr (cdr Expr)) Env)) ]
    [ (equal? (car Expr) '*)  (* (semArith (cadr Expr) Env)
                                 (semArith (cadr (cdr Expr)) Env)) ]
    [ (equal? (car Expr) '/)  (/ (semArith (cadr Expr) Env)
                                 (semArith (cadr (cdr Expr)) Env)) ]
    #| ADDED for hw5: anonymous functions |#
    [ (equal? (car Expr) 'anonf) (semanon (car (cadr Expr))
                                          (cadr (cadr Expr))
                                          (semArithList (cadr (cdr Expr)) Env)
                                          Env) ]
    ))

(define (semanon ParamList Expr ArgList Env)
  (semArith Expr (genEnv ParamList ArgList Env)))

; semantics of complex conditions
(define (semcomplexcond CCond Env)
  (cond
    [ (equal? (car CCond) 'or)   (or (semcomplexcond (cadr CCond) Env)
                                     (semcomplexcond (cadr (cdr CCond)) Env)) ]
    [ (equal? (car CCond) 'and)   (and (semcomplexcond (cadr CCond) Env)
                                     (semcomplexcond (cadr (cdr CCond)) Env)) ]
    [ (equal? (car CCond) 'not)   (not (semcomplexcond (cadr CCond) Env))
                                      ]
    [ else  (semboolcond CCond Env) ]))  ;; semantics of conditions: lt, gt

; complete the definition
(define (semboolcond BCond Env)
  (cond
    [ (equal? (car BCond) 'gt)  (> (semArith (cadr BCond) Env)
                                   (semArith (cadr (cdr BCond)) Env)) ]
    [ (equal? (car BCond) 'lt)  (< (semArith (cadr BCond) Env)
                                   (semArith (cadr (cdr BCond)) Env)) ]
    [ (equal? (car BCond) 'eq)  (equal? (semArith (cadr BCond) Env)
                                        (semArith (cadr (cdr BCond)) Env)) ]))


