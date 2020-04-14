#lang racket
(provide (all-defined-out))
(require racket/trace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#| TODO: Prop is working, need to implement (AND OR NOT) next...  |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| HELPERS |#

(define (length lst)
  (if (null? lst)
      0  ; Base Case
      (+ 1 (length (cdr lst)))  ; Recursive Case
      )
  )

(define (add2end e lst)
  (if (null? lst)
      (cons e '())
      (cons (car lst) (add2end e (cdr lst)))))

(define (verticies ists return)
  (if (null? ists)
      return
      (verticies (cdr ists) (cons (car (car ists)) return)))
  )
    
(define (var_in_sts? var ists_props)
  (if (null? ists_props)
      false
      (if (equal? var (car ists_props))
          true
          (var_in_sts? var (cdr ists_props))
          )
      )
  )

#| PROP |#
(define (doprop var ists return)
  (if (null? ists)
      return
      (if (var_in_sts? var (second (car ists)))
          (doprop var (cdr ists) (cons (first (car ists)) return))
          (doprop var (cdr ists) return)
          )
      )
  )
 

#| DO TR FA |#
(define (do_trfa trfa ists)
  (cond
    [(equal? trfa `fa) `() ]
    [(equal? trfa `tr) (verticies ists `()) ] ))


#| DO AND |#
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


#| DO OR |#
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

#| DO NOT |#
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
      
      [(equal? (car todo) `ax) true]
      [(equal? (car todo) `ex) true]
      
      [(equal? (car todo) `starax) true]
      [(equal? (car todo) `starex) true]
  
    ))



#| Test Code |#
(define sts
  `( (s0 (p q))
     (s1 (p))
     (s2 (q))
     (s3 ())))

(define trs
  `( (s0 s1)
     (s0 s2)
     (s0 s3)
     (s1 s1)
     (s2 s3)
     (s3 s1)
     ))

;(vsem `tr sts trs)
;; ==> `(s0 s1 s2 s3)

;(vsem `fa sts trs)
;; ==> `()

;(vsem `(prop p) sts trs)
;(vsem `(prop q) sts trs)

;(vsem `(and (prop p) (prop q)) sts trs)
;(vsem `(and (prop t) (prop q)) sts trs)
;(vsem `(and (prop q) (prop p)) sts trs)
;(vsem `(or (prop p) (prop q)) sts trs)
;(vsem `(or (prop t) (prop q)) sts trs)
;(vsem `(or (prop q) (prop p)) sts trs)

(vsem `(not (prop q)) sts trs)
(vsem `(not (or (prop p) (prop q))) sts trs)
(vsem `(or (prop p) (prop q)) sts trs)
;; ==> ’(s0 s1 s2)

;(vsem ’(ex (prop p)) sts trs)
;; ==> ’(s0 s1 s3)

;(vsem ’(ax (prop p)) sts trs)
;; ==> ’(s1 s3)

;(vsem ’(starex (prop p)) sts trs)
;; ==> ’(s2 s3 s0 s1)

;(vsem ’(starax (prop p)) sts trs)
;; ==> ’(s0 s1 s2 s3)


#| Examples of valid expressions |#
;(define e1 `tr)
;(define e2 `fa)
;(define e3 `(or (prop x) (prop y)))
;(define e4 `(starax (starex (or (prop x) (not (ex (prop y)))))))

