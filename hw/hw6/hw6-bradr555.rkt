#lang racket
(require racket/trace)
(provide (all-defined-out))
(require "program.rkt")


#| ###################################################################### |#
;; semantics of a program = semantics of rest of the program in the context of semantics of first statement
(define (sem P Env Heap)
  (if (null? (cdr P))
      (cons (semstmt (car P) Env Heap) (list (heap_ops (car P) Env Heap)))
      (sem (cdr P) (semstmt (car P) Env Heap) (heap_ops (car P) Env Heap))))


(define (semstmt S Env Heap )
  (cond
    [ (equal? (car S) 'decl)   (cons (list (cadr S) 0) Env) ]
    [ (equal? (car S) 'assign) (updateValue (cadr S)(semArith (cadr (cdr S)) Env) Env) ]
    [ (equal? (car S) 'if)  (removemarker (semIf (semcomplexcond (cadr S) Env)(cadr (cdr S))(cons (list '$m 0) Env))) ]
    [ (equal? (car S) 'while) (if (semcomplexcond (cadr S) Env)(sem (list (list 'if '(eq 1 1) (cadr (cdr S))) S)Env) Env) ]    
    [ (equal? (car S) 'fundecl) (cons (list (cadr S) (cadr (cdr S))) Env) ]
    [ (equal? (car S) 'call)  (removemarker (semcall(findDef (car (cadr S)) (length (cadr (cadr S))) Env Env)(semArithList (cadr (cadr S)) Env))) ]
        

    [(equal? (car S) `deref) (updateValue (cadr S) (doderef (findValue (third S) Env) Heap) Env)]
    ; [ (equal? (car expr) 'deref) (cons (evaldref
    ;                                     (findval(car(eval (second expr) env heap)))
    ;                                     (second (eval (second expr) env heap)))
    ;                                    (list (second (eval (second expr) env heap))))
    [(equal? (car S) `ref) (updateValue (second S) (find_free Heap) Env) ]
    [(equal? (car S) `free)  true]
    [(equal? (car S) `wref)  true]
    [(equal? (car S) `anonf) true]
    )
  )

(define (heap_ops S Env Heap)
  (cond
    [(equal? (car S) `ref)(doref (second S) (semArith (third S) Env) Env Heap) ]
    [else Heap]
    )
  )

; return the value at loc
(define (doderef loc heap)
  (if (null? heap)
      `ooma
      (if (equal? loc (car (car heap)))
          (if (equal? `free (second (car heap)))
              `fma
              (second (car heap)))
          (doderef loc (cdr heap)))))


  

#| ############################################## DO REF############################################################ |#
  (define (doref symb val env heap)
    (if (equal? `oom (find_free heap))
        (cons `oom (list heap)) ; RETURN oom and the heap -- TODO write exception stuff
        (write_free (find_free heap) val heap))) ; Return location and updated heap
#| ################################################################################################################# |#
#|TESTING |#
(define heap1 `((1 free) (2 free)))
#| ############################################### Working Helpers ################################################# |#

; Find the first occurance of a `free in the heap...
(define (find_free heap)
  (if (null? heap)
      `oom
      (if (equal? `free (second (car heap)))
          (car (car heap))
          (find_free (cdr heap)))))

(define (write_free loc val heap)
  (if (equal? loc (car (car heap)))
      (cons (list (car (car heap)) val) (cdr heap))
      (cons (car heap) (write_free loc val (cdr heap)))))

#| ################################################################################################################# |#

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
      (cons (list (car (car Env)) val)   (cdr Env))
      (cons (car Env) (updateValue v val (cdr Env)))))


(define (semArith Expr Env)
  (cond
    [ (number? Expr)          Expr ]
    [ (symbol? Expr)          (findValue Expr Env) ]
    [ (equal? (car Expr) '+)  (+ (semArith (cadr Expr) Env) (semArith (cadr (cdr Expr)) Env)) ]
    [ (equal? (car Expr) '-)  (- (semArith (cadr Expr) Env) (semArith (cadr (cdr Expr)) Env)) ]
    [ (equal? (car Expr) '*)  (* (semArith (cadr Expr) Env) (semArith (cadr (cdr Expr)) Env)) ]
    [ (equal? (car Expr) '/)  (/ (semArith (cadr Expr) Env) (semArith (cadr (cdr Expr)) Env)) ]
    #| ADDED for hw5: anonymous functions |#
    [ (equal? (car Expr) 'anonf) (semanon (car (cadr Expr))
                                          (cadr (cadr Expr))
                                          (semArithList (cadr (cdr Expr)) Env)
                                          Env) ] ))

(define (semanon ParamList Expr ArgList Env) (semArith Expr (genEnv ParamList ArgList Env)))

; semantics of complex conditions
(define (semcomplexcond CCond Env)
  (cond
    [ (equal? (car CCond) 'or)    (or  (semcomplexcond (cadr CCond) Env) (semcomplexcond (cadr (cdr CCond)) Env)) ]
    [ (equal? (car CCond) 'and)   (and (semcomplexcond (cadr CCond) Env) (semcomplexcond (cadr (cdr CCond)) Env)) ]
    [ (equal? (car CCond) 'not)   (not (semcomplexcond (cadr CCond) Env))]
    [ else  (semboolcond CCond Env) ]))  ;; semantics of conditions: lt, gt

(define (semboolcond BCond Env)
  (cond
    [ (equal? (car BCond) 'gt)  (> (semArith (cadr BCond) Env) (semArith (cadr (cdr BCond)) Env)) ]
    [ (equal? (car BCond) 'lt)  (< (semArith (cadr BCond) Env) (semArith (cadr (cdr BCond)) Env)) ]
    [ (equal? (car BCond) 'eq)  (equal? (semArith (cadr BCond) Env) (semArith (cadr (cdr BCond)) Env)) ]))



#| Tests |#
(display "TESTS:\n")
; (sem program environment heap)
(sem p0 `() `((1 free) (2 free)) )
(display "\n==> `(((y 10) (x 1)) ((1 10) (2 free)))\n") 


