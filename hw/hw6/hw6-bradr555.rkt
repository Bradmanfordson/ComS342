#lang racket
(require racket/trace)
(provide (all-defined-out))
(require "program.rkt")


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
    [ (equal? (car S) 'while) (if (semcomplexcond (cadr S) Env)(sem (list (list 'if '(eq 1 1) (cadr (cdr S))) S)Env Heap) Env) ]    
    [ (equal? (car S) 'fundecl) (cons (list (cadr S) (cadr (cdr S))) Env) ]
    [ (equal? (car S) 'call)  (removemarker (semcall(findDef (car (cadr S)) (length (cadr (cadr S))) Env Env)(semArithList (cadr (cadr S)) Env))) ]
     
    [(equal? (car S) `deref) (updateValue (cadr S) (doderef (findValue (third S) Env) Heap) Env)]
    [(equal? (car S) `ref)   (updateValue (second S) (find_free Heap) Env) ]
    [else Env]
    )
  )

(define (heap_ops S Env Heap)
  (cond
    [(equal? (car S) `ref) (doref (second S) (semArith (third S) Env) Env Heap) ]
    [(equal? (car S) `wref) (dowref (semArith (second S) Env) (semArith (third S) Env) Heap)]
    [(equal? (car S) `free) (dofree (semArith (second S) Env) Heap)]
    [else Heap]
    )
  )


#| WORKING HELPERS |#

; Find the first occurance of a `free in the heap...
(define (find_free heap)
  (if (null? heap)
      (cons `oom)
      (if (equal? `free (second (car heap)))
          (car (car heap))
          (find_free (cdr heap)))))

(define (write_loc loc val heap)
  (if (equal? loc (car (car heap)))
      (cons (list (car (car heap)) val) (cdr heap))
      (cons (car heap) (write_loc loc val (cdr heap)))))

(define (write_heap loc val heap)
  (if (null? heap)
      (cons `ooma) ;TODO
      (if (equal? loc (car (car heap)))
          (if (equal? `free (second (car heap)))
              (cons `fma) ;TODO
              (cons (list (car (car heap)) val) (cdr heap)))
          (if (equal? `ooma (write_heap loc val (cdr heap)))
              (cons `ooma) ;TODO
              (if (equal? `fam (write_heap loc val (cdr heap)))
                  (cons `fma);TODO
                  (cons (car heap) (write_heap loc val (cdr heap))))))))


#| ###################################################################### |#

#| HW6 |#
(define (dofree loc heap)
  (if (equal? 'fma (write_loc loc `free heap))
      (cons `fma)
      (if (equal? `ooma (write_loc loc `free heap))
          (cons `ooma)
          (write_loc loc `free heap))))

(define (dowref loc val heap)
  (if (equal? `fma (write_heap loc val heap))
      (cons `fma)
      (if (equal? `ooma (write_heap loc val heap))
          (cons `ooma)
          (write_heap loc val heap))))

; return the value at loc
(define (doderef loc heap)
  (if (null? heap)
      (cons `ooma) ; TODO -- this is wrong... but works if this isnt caught
      (if (equal? loc (car (car heap)))
          (if (equal? `free (second (car heap)))
              (cons `fma) ; TODO -- this is wrong... but works if this isnt caught
              (second (car heap)))
          (doderef loc (cdr heap)))))

(define (doref symb val env heap)
  (if (equal? `oom (find_free heap))
      (cons `oom) ; RETURN oom and the heap -- TODO write exception stuff
      (write_loc (find_free heap) val heap))) ; Return location and updated heap

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
(display "==> `(((y 10) (x 1)) ((1 10) (2 free)))\n\n")

(sem p0 `() `((1 20) (2 free)))
(display "==> `(((y 10) (x 2)) ((1 20) (2 10)))\n\n")

;(sem p0 `() `((1 20) (2 40)))
;(display "==> `(((y 0) (x 0)) (oom))\n\n")


(sem p1 `() `((1 free) (2 free)))
(display "==> `(((y 30) (x 1)) ((1 free) (2 free)))\n\n")

;(sem p5 `() `((1 free)))
;(display "==> `(((x 0)) (ooma))\n\n")

;(sem p6 `() `((1 free)))
;(display "==> `(((x 0)) (fma))\n\n")

;(sem p2 `() `((1 20) (2 500)))



(define t1
  '(
    (decl x)
    (decl t)
    (assign t 1)
    (fundecl (f (y)) (
                      (assign y (+ x y))
                      (deref x y)
                      ))
    (while (lt t 3) (
                     (call (f (t)) 1)
                     (assign t (+ t 1))
                     ))
    )
  )

;(sem t1 `() `((1 free)))
;; '() '((1 2) (4 5))  =>  '((((f (y)) ((assign y (+ x y)) (deref x y))) (t 3) (x 5)) ((1 2) (4 5)))
;; '() '((1 free)) => '((((f (y)) ((assign y (+ x y)) (deref x y))) (t 1) (x 0)) (fma))
;; '() '((1 5)) => '((((f (y)) ((assign y (+ x y)) (deref x y))) (t 2) (x 5)) (ooma))

(define t2
  '(
    (decl x)
    (assign x 5)
    (ref x (+ x x))
    )
  )

(sem t2 `() `((1 free)))
(sem t2 `() '((1 10) (2 free) (3 free)))
;(sem t2 '() '((1 10) (2 5) (3 1)))
;; '() '((1 free)) => '(((x 1)) ((1 10)))
;; '() '((1 10) (2 free) (3 free)) => '(((x 2)) ((1 10) (2 10) (3 free))
;; '() '((1 10) (2 5) (3 1)) => '(((x 5)) (oom))

(define t3
  '(
    (decl x)
    (assign x 1)
    (decl y)
    (assign y 4)
    (while (lt x y) (
                     (wref x (+ x y))
                     (assign x (+ x 1))
                     ))
    )
  )

;(sem t3 '() '((1 0) (2 0) (3 0)));
;; '() '((1 0) (2 0) (3 0)) => '(((y 4) (x 4)) ((1 5) (2 6) (3 7)))
;; '() '((1 0) (2 0) (3 free)) => '(((y 4) (x 3)) (fma))
;; '() '((1 0)) => '(((y 4) (x 2)) (ooma))

(define t4
  '(
    (decl x)
    (decl y)
    (assign y 1)
    (while (lt x 4) (
                     (free (+ x y))
                     (assign x (+ x 1))
                     )
           )
    )
  )
;(sem t4 '() '((1 0) (2 free) (3 5) (4 100)))
;; '() '((1 0) (2 free) (3 5) (4 100)) => '(((y 1) (x 4)) ((1 free) (2 free) (3 free) (4 free)))
;; '() '((1 0) (2 free)) => '(((y 1) (x 2)) (ooma))

(define t5
  '(
    (decl x)
    (fundecl (f (y)) (
                      (ref x (* y y))
                      ))
    (decl y)
    (assign y 1)
    (while (lt x 5) (
                     (call (f (y)) 1)
                     (assign y (+ y 1))
                     ))))
;; '() '((1 free) (2 free) (3 free) (4 free) (5 free)) => '(((y 6) ((f (y)) ((ref x (* y y)))) (x 5)) ((1 1) (2 4) (3 9) (4 16) (5 25)))
;; '() '((1 10) (2 3) (3 23) (4 1) (5 free)) => '(((y 2) ((f (y)) ((ref x (* y y)))) (x 5)) ((1 10) (2 3) (3 23) (4 1) (5 1)))
;; '() '((1 free)) => '(((y 2) ((f (y)) ((ref x (* y y)))) (x 1)) (oom))

(define t6
  '(
    (fundecl (func (z)) (
                         (decl temp)
                         (ref temp 0)
                         (wref z temp)
                         ))
    (call (func (y)) 1)
    (call (func (x)) 1)
    (call (func (t)) 1)
    )
  )
;; '((y 1) (x 5) (t 10)) '((1 5) (2 free) (3 free) (5 6) (6 free) (10 7)) => '((((func (z)) ((decl temp) (ref temp 0) (wref z temp))) (y 1) (x 5) (t 10)) ((1 2) (2 0) (3 0) (5 3) (6 0) (10 6)))

(define t7
  '(
    (decl x)
    (assign x 6)
    (ref x (* x x))
    (deref x (+ x 2))
    (wref x (* x x))
    (free (* (* x x) x))
    )
  )
;(sem t7 '() '((2 0) (4 free) (6 2) (8 10)))
;; '() '((2 0) (4 free) (6 2) (8 10)) => '(((x 2)) ((2 4) (4 36) (6 2) (8 free))

