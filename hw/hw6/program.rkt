#lang racket
(provide (all-defined-out))


(define p0
  `(
    (decl x)
    (decl y)
    (ref x 10)
    (deref y x)
    )
  )
; > (sem p0 `() `((1 free) (2 free)))
; `(((y 10) (x 1))
;((1 10) (2 free)))
; > (sem p0 `() `((1 20) (2 free)))
; `(((y 10) (x 2))
;((1 20) (2 10)))
; > (sem p0 `() `((1 20) (2 40)))
; `(((y 0) (x 0))
;(oom))
(define p1
  `(
    (decl x)
    (decl y)
    (ref x 10)
    (wref x 30)
    (deref y x)
    (free x)
    )
  )
; > (sem p1 `() `((1 free) (2 free)))
; `(((y 30) (x 1)) ((1 free) (2 free)))
(define p5
  `(
    (decl x)
    (assign x 0)
    (free x)
    )
  )
; > (sem p5 `() `((1 free)))
;`(((x 0)) (ooma))
(define p6
  `(
    (decl x)
    (deref x 1)
    )
  )
; > (sem p6 `() `((1 free)))
; `(((x 0)) (fma))
(define p2
  `(
    (fundecl (swap (x y)) (
                           (decl temp1)
                           (decl temp2)
                           (deref temp1 x)
                           (deref temp2 y)
                           (wref x temp2)
                           (wref y temp1)
                           )
             )
    (decl a)
    (decl b)
    (assign a 1)
    (assign b 2)
    (call (swap (a b)) 1)
    )
  )
; > (sem p2 `() `((1 20) (2 500)))
; `(((b 2) (a 1)
;
;((swap (x y))
 ;
; ((decl temp1)
  ;;
  ;(decl temp2)
  ;
  ;(deref temp1 x)
  ;
 ; (deref temp2 y)
  ;
;  (wref x temp2)
  ;
;  (wref y temp1)))
;
;)
;
;((1 500) (2 20)))
;












;;; #lang racket
;;; (provide (all-defined-out))

;;; ;; exception: heap '()
;;; ;; exception: heap '((1 free))
;;; ;; exception: heap '((1 22))
;;; ;; 3 '((1 10) (2 22) (2 1)): heap '((1 free) (2 22) (3 free))
;;; ;; 4
;;; (define prog1
;;;   '(ref (ref 10)))

;;; ;; '(30 ((1 10) (2 20))): heap '((1 free) (2 free))
;;; ;; 2
;;; (define prog2
;;;   '(let (x (ref 10))
;;;      (let (y (deref x))
;;;        (let (x (ref 20))
;;;          (let (z (deref x))
;;;            (+ y z))))))


;;; ;; '((exception runtime): heap '((1 free) (2 free))
;;; (define prog3
;;;   '(let (x (ref 10))
;;;      (let (y (ref 20))
;;;        (let (z (deref (+ x y)))
;;;          z))))

;;; ;; '(1 ((1 20) (2 free))): heap ((1 free) (2 free))
;;; (define prog4
;;;   '(let (x (ref 10))
;;;      (let (y (free x))
;;;        (ref 20))))

;;; ;; '(1 ((1 20))): heap ((1 free))
;;; (define prog5
;;;   '(let (x (ref 10))
;;;      (let (y (free (+ x (- 1 1))))
;;;        (ref 20))))

;;; ;; '(1 ((1 1))): heap ((1 free))
;;; (define prog6
;;;   '(wref 1 (ref 20)))

;;; ;;'(21 ((1 21))): heap ((1 free))
;;; (define prog7
;;;   '(let (x (wref 1 (+ 20 (ref  20))))
;;;      (deref 1)))

;;; ;;'(160 ((1 40) (2 80))): heap ((1 free) (1 free))
;;; (define prog8
;;;   '(let (x (ref 10))
;;;      (let (y (ref 20))
;;;        (+ (wref x 40) (+ (wref y 80) (deref x))))))

;;; ;; exception: 
;;; (define prog9
;;;   '(let (x (ref 10))
;;;      (let (y (ref 20))
;;;        (+ (wref x 40) (+ (free y) (deref y))))))

;;; ;; '(40 ((1 20))): heap '((1 free))
;;; (define prog10
;;;   '((gt (ref 10) 0)
;;;     (+ (wref 1 20)
;;;        (deref 1))
;;;     2))

;;; ;; '(30 ((1 20))): heap '((1 free))
;;; (define prog11
;;;   '((gt (ref 10) 0)
;;;     (+ (deref 1)
;;;        (wref 1 20))
;;;     2))

