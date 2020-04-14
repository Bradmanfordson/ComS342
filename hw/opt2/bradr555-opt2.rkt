#lang racket
(provide (all-defined-out))
(require racket/trace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#| TODO: Prop is working, need to implement (AND OR NOT) next...  |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| HELPERS |#

(define (verticies ists return)
  (if (null? ists)
      return
      (verticies (cdr ists) (cons (car (car ists)) return)))
  )
     

#| DO THIS... |#
(define (do_trfa trfa ists)
  (cond
    [(equal? trfa `fa) `() ]
    [(equal? trfa `tr) (verticies ists `()) ] ))



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

(define (var_in_sts? var ists_props)
  (if (null? ists_props)
      false
      (if (equal? var (car ists_props))
          true
          (var_in_sts? var (cdr ists_props))
          )
      )
  )


#| VSEM |#
(define (vsem trfa ists itrs)
   ; (if (null? (cdr tr_fa))  ; one statement in the program 
     ; (semstmt (car P))  ;; find the semantics of the single statement
    ;  (vsem (cdr P) (semstmt (car P))))  ;; else relay-chain the semantics
  (if (list? trfa)
      (semstmt trfa ists itrs)
      (do_trfa trfa ists))
  )

(define (semstmt todo ists itrs)
  (cond
      [(equal? (car todo) `prop) (doprop (second todo) ists `())];(var_in_sts? (second  todo) ists)]
      
      [(equal? (car todo) `not) true]
      [(equal? (car todo) `and) true]
      [(equal? (car todo) `or) true]
      
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

(vsem `tr sts trs)
;; ==> `(s0 s1 s2 s3)

(vsem `fa sts trs)
;; ==> `()

(vsem `(prop p) sts trs)
(vsem `(prop q) sts trs)

;(vsem ’(or (prop p) (prop q)) sts trs)
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

