#lang racket
(provide (all-defined-out))
(require "tests.rkt")

#| Allen Roberts |#

#| HELPERS |#
(define (add2end e lst)
  (if (null? lst)
      (cons e '())
      (cons (car lst) (add2end e (cdr lst)))))

(define (verticies ists return)
  (if (null? ists)
      return
      (verticies (cdr ists) (cons (car (car ists)) return))))
    
(define (var_in_sts? var ists_props)
  (if (null? ists_props)
      false
      (if (equal? var (car ists_props))
          true
          (var_in_sts? var (cdr ists_props)))))

#| PROP |#
(define (doprop var ists return)
  (if (null? ists)
      return
      (if (var_in_sts? var (second (car ists)))
          (doprop var (cdr ists) (cons (first (car ists)) return))
          (doprop var (cdr ists) return))))
 
#| TR FA |#
(define (do_trfa trfa ists)
  (cond
    [(equal? trfa `fa) `() ]
    [(equal? trfa `tr) (verticies ists `()) ]))

#| AND |#
(define (doand prop1 prop2 return)
  (if (null? prop1)
      return
      (if (not (list? prop1))
          (if (var_in_sts? prop1 prop2)
              (cons prop1 return)
              return
              )
          (if (var_in_sts? (car prop1) prop2)
              (doand (cdr prop1) prop2 (cons (car prop1) return))
              (doand (cdr prop1) prop2 return)))))

#| OR |#
(define (door prop1 prop2)
  (if (null? prop1)
      prop2
      (if (not (list? prop1))
          (if (var_in_sts? prop1 prop2)
              prop2
              (add2end prop1 prop2))
          (if (var_in_sts? (car prop1) prop2)
              (door (cdr prop1) prop2)
              (door (cdr prop1) (add2end (car prop1) prop2))))))

#| NOT |#
(define (donot prop ists return)
  (if (null? ists)
      return
      (if (not (list? ists))
          (if (var_in_sts? ists prop)
              return
              (add2end ists return)
              )
          (if (var_in_sts? (car ists) prop)
              (donot prop (cdr ists) return)
              (donot prop (cdr ists) (add2end (car ists) return))))))
             
#| DO EX |#
; Iterate through TRS finding add (S0 X)
; THEN iterate again finding all (X S0) and which ever X has both? this doesnt make any sense

#| VSEM |#
(define (vsem trfa ists itrs)
  (if (list? trfa)
      (semstmt trfa ists itrs)
      (do_trfa trfa ists))
  )

(define (semstmt todo ists itrs)
  (cond
      [(equal? (car todo) `prop) (doprop (second todo) ists `())];(var_in_sts? (second  todo) ists)]
      
      [(equal? (car todo) `not) (donot (semstmt (second todo) ists itrs) (verticies ists `() ) `())]
      [(equal? (car todo) `and) (doand (semstmt (second todo) ists itrs) (semstmt (third todo) ists itrs) `() )]
      [(equal? (car todo) `or)  (door  (semstmt (second todo) ists itrs) (semstmt (third todo) ists itrs) )]

      [(equal? (car todo) `ex) (doprop (second (second todo)) ists `() )]
      [(equal? (car todo) `ax) (semstmt `(not (ex (not (second todo)))) ists itrs)] ; this is correct but doesnt matter because idk how to do ex
      
      [(equal? (car todo) `starax) (verticies ists `() )]
      [(equal? (car todo) `starex) (verticies ists `() )]))
