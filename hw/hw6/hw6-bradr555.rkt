#lang racket
(require "program.rkt")
(provide (all-defined-out))


;; semantics of a program = semantics of rest of the program in the context of semantics of first statement
;; HW6: Edited in order to properly handle a seperate Env and Heap
;; basically, we're running semantics of a program statement on both the environment and heap seperately
;; and updated each as needed
(define (sem P Env Heap)
  (if (null? (cdr P))
      (cons (semstmt (car P) Env Heap) (list (heap_ops (car P) Env Heap))) 
      (sem (cdr P) (semstmt (car P) Env Heap) (heap_ops (car P) Env Heap))))


;; HW6: edited to include looking for exceptions and to pass heap stuff around
(define (semstmt S Env Heap )
  (cond
    ;; Exception handling, basically if an exception occurs it'll be written to the heap and basically
    ;; keeps any other code from executing once an exception happens
    [ (equal? (car Heap) `oom) Env]
    [ (equal? (car Heap) `ooma) Env]
    [ (equal? (car Heap) `fma) Env]
    [ (equal? (car S) 'decl)   (cons (list (cadr S) 0) Env) ]
    [ (equal? (car S) 'assign) (updateValue (cadr S)(semArith (cadr (cdr S)) Env) Env) ]
    [ (equal? (car S) 'if)  (removemarker (car (semIf (semcomplexcond (cadr S) Env)(cadr (cdr S))(cons (list '$m 0) Env) Heap))) ]
    [ (equal? (car S) 'while) (if (semcomplexcond (cadr S) Env)(sem (list (list 'if '(eq 1 1) (cadr (cdr S))) S)Env Heap) Env) ]    
    [ (equal? (car S) 'fundecl) (cons (list (cadr S) (cadr (cdr S))) Env) ]
    [ (equal? (car S) 'call)  (removemarker (car (semcall(findDef (car (cadr S)) (length (cadr (cadr S))) Env Env)(semArithList (cadr (cadr S)) Env) Heap))) ]

    ;; deref logic to update the environment
    [(equal? (car S) `deref) (if (or (equal? (list `ooma) (doderef (findValue (third S) Env) Heap))
                                     (equal? (list `fma) (doderef (findValue (third S) Env) Heap)))
                                     Env
                                     (updateValue (cadr S) (doderef (findValue (third S) Env) Heap) Env))]
    ;; ref logic to update the environment
    [(equal? (car S) `ref)   (if (equal? (find_free Heap) `oom)
                                 Env
                                 (updateValue (second S) (find_free Heap) Env)) ]
    [else Env]
    )
  )

;; semstmt but for heap operations
(define (heap_ops S Env Heap)
  (cond
    [ (equal? (car Heap) `oom) Heap]
    [ (equal? (car Heap) `ooma) Heap]
    [ (equal? (car Heap) `fma) Heap]
    [(equal? (car S) `ref)   (doref (second S) (semArith (third S) Env) Env Heap) ]
    [(equal? (car S) `wref)  (dowref (semArith (second S) Env) (semArith (third S) Env) Heap)]
    [(equal? (car S) `free)  (dofree (semArith (second S) Env) Heap)]
    [(equal? (car S) `deref) (if (equal? (list `ooma) (doderef (findValue (third S) Env) Heap))
                                 (list `ooma)
                                 (if (equal? (list `fma) (doderef (findValue (third S) Env) Heap))
                                     (list `fma)
                                     Heap))]
    ;; Added this because the heap wasn't being properly updated, by doing this, it is
    [ (equal? (car S) 'call)  (car (cdr (semcall(findDef (car (cadr S)) (length (cadr (cadr S))) Env Env)(semArithList (cadr (cadr S)) Env) Heap))) ]

    [else Heap]
    )
  )
#| WORKING HELPERS for HW6, hopefully exceptions are actually working properly, seem to be from tests |#

(define (find_free heap)
  (if (null? heap)
      `oom
      (if (equal? `free (second (car heap)))
          (car (car heap))
          (find_free (cdr heap)))))

(define (write_loc loc val heap)
  (if (null? heap)
      (list `ooma)
      (if (equal? loc (car (car heap)))
          (cons (list (car (car heap)) val) (cdr heap))
          (cons (car heap) (write_loc loc val (cdr heap)))))
  )

(define (write_heap loc val heap)
  (if (null? heap)
      (list `ooma)
      (if (equal? loc (car (car heap)))
          (if (equal? `free (second (car heap)))
              (list `fma) 
              (cons (list (car (car heap)) val) (cdr heap)))
          (if (equal? `ooma (write_heap loc val (cdr heap)))
              (list `ooma) 
              (if (equal? `fam (write_heap loc val (cdr heap)))
                  (list `fma)
                  (cons (car heap) (write_heap loc val (cdr heap))))))))

(define (dofree loc heap)
  (if (equal? (list (car heap) `fma) (write_heap loc `free heap))
      (list `fma)
      (if (equal? (write_heap loc `free heap)  (list (car heap )`ooma) )
          (list `ooma)
          (write_heap loc `free heap))))

(define (dowref loc val heap)
  (if (equal? (list `fma) (write_heap loc val heap))
      (list `fma)
      (if (equal? (list `ooma) (write_heap loc val heap))
          (list `ooma)
          (write_heap loc val heap))))

(define (doderef loc heap)
  (if (null? heap)
      (list `ooma) ; 
      (if (equal? loc (car (car heap)))
          (if (equal? `free (second (car heap)))
              (list `fma)
              (second (car heap)))
          (doderef loc (cdr heap)))))


(define (doref symb val env heap)
  (if (equal? `oom (find_free heap))
      (list `oom) 
      (write_loc (find_free heap) val heap)))

#| ###################################################################### |#


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
(define (semcall ParamsDefEnv Args Heap)
  (sem (cadr ParamsDefEnv)        ;; semantics of the definition 
       (genEnv (car ParamsDefEnv) ;; genEnv creates the environment by adding mapping of params to argval
               Args
               (cadr (cdr ParamsDefEnv)))
       Heap))


(define (findValue v Env) ;; update to make room for function names being assigned to variables; they do not
  ;; have values
  (if (null? Env)         ;; couple of lines to add: ADDED for hw5
      v
      (if (equal? v (car (car Env)))
          (cadr (car Env))
          (findValue v (cdr Env)))))

#| The addendum for the function calls ends here |#

(define (semIf condVal SSeq Env Heap)
  (if condVal
      (sem SSeq Env Heap)
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


