#lang racket
(provide (all-defined-out))

#| ====================== SYNCHK CHECKS ========================= |#
(define program1
  '(()))
;; #f

(define program2
  '(decl x))
;; #f

(define program3
  '((function y)))
;; #f

;; DECL TESTS
(define program4
  '((decl)))
;; #f

(define program5
  '((decl x 1)))
;; #f

(define program6
  '((decl x)))
;; #t

;; ASSIGN TESTS
(define program7
  '((assign x 5)))
;; #t

(define program8
  '((assign x y z)))
;; #f

(define program9
  '((assign (x) y)))
;; #f

(define program10
  '((assign 3 y)))
;; #f

(define program11
  '((assign x (gt x y))))
;; #f

(define program12
  '((assign x (decl y))))
;; #f

(define program13
  '((assign x y)))
;; #t

(define program14
  '((assign x (+))))
;; #f

(define program15
  '((assign x (+ (- x y)))))
;; #f

(define program16
  '((assign x (+ (- x y) z))))
;; #t

(define program17
  '((assign x (- (decl x) y))))
;; #f

;; IF TESTS
(define program18
  '((if or (decl x))))
;; #f

(define program19
  '((if (or (gt x y) (and (gt x y) (gt x y))) ((decl x)))))
;; #t

(define program20
  '((if (or (and (gt x y) (gt x y)) (gt x y)) ((decl x)))))
;; #t

(define program21
  '((if (not (gt x y)) ((decl x)))))
;; #t

(define program22
  '((if (not (gt x y)) (decl x))))
;; #f

(define program23
  '((if (not (gt x y) (gt x y)) ((decl x)))))
;; #f

(define program24
  '((if (gt x (or (gt x y) (gt x y))) ((decl x)))))
;; #f

(define program25
  '((if (gt x y) ((decl x)))))
;; #t

;; WHILE TESTS
(define program26
  '((while or (decl x))))
;; #f

(define program27
  '((while (or (gt x y) (and (gt x y) (gt x y))) ((decl x)))))
;; #t

(define program28
  '((while (or (and (gt x y) (gt x y)) (gt x y)) ((decl x)))))
;; #t

(define program29
  '((while (not (gt x y)) ((decl x)))))
;; #t

(define program30
  '((while (not (gt x y)) (decl x))))
;; #f

(define program31
  '((while (not (gt x y) (gt x y)) ((decl x)))))
;; #f

(define program32
  '((while (gt x (or (gt x y) (gt x y))) ((decl x)))))
;; #f

(define program33
  '((while (gt x y) ((decl x)))))
;; #t
