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

#| ============================ SEM TESTS =========================== |#

(define program34
  '((decl x)))
;; '() => '((x 0))
;; '((y 0)) => '((x 0) (y 0))
;; '((x 3)) => '((x 0) (x 3))

(define program35
  '((assign x (+ 6 x))))
;; '((x 0)) => '((x 6))
;; '((x 1)) => '((x 7))
;; '((x -1)) => '((x 5))

(define program36
  '((assign x (+ 7 (- x y)))))
;; '((x 0) (y 1)) => '((x 6) (y 1))

(define program37
  '(
    (decl x)
    (assign x 6)
    (decl y)
    (assign y 7)
    (if (gt y x)
        ((assign x 8)))))
;; '() => '((y 7) (x 8))

(define program38
  '(
    (decl x)
    (assign x 7)
    (decl y)
    (assign y 6)
    (if (gt y x)
        ((assign x 8)))))
;; '() => '((y 6) (x 7))

(define program39
  '(
    (decl x)
    (assign x 6)
    (decl y)
    (assign y 7)
    (if (lt y x)
        ((assign x 8)))))
;; '() => '((y 7) (x 6))

(define program40
  '(
    (decl x)
    (assign x 7)
    (decl y)
    (assign y 6)
    (if (lt y x)
        ((assign x 8)))))
;; '() => '((y 6) (x 8))

(define program41
  '(
    (decl x)
    (assign x 6)
    (decl y)
    (assign y 7)
    (if (eq y x)
        ((assign x 8)))))
;; '() => '((y 7) (x 6))

(define program42
  '(
    (decl x)
    (assign x 7)
    (decl y)
    (assign y 7)
    (if (eq y x)
        ((assign x 8)))))
;; '() => '((y 7) (x 8))

(define program43
  '(
    (decl x)
    (assign x 6)
    (decl y)
    (assign y 7)
    (if (or (gt y x) (lt y x))
        ((assign x 8)))))
;; '() => '((y 7) (x 8))

(define program44
  '(
    (decl x)
    (assign x 6)
    (decl y)
    (assign y 6)
    (if (or (gt y x) (lt y x))
        ((assign x 8)))))
;; '() => '((y 6) (x 6))

(define program45
  '(
    (decl x)
    (assign x 6)
    (decl y)
    (assign y 7)
    (decl z)
    (assign z 5)
    (if (and (gt y x) (lt z x))
        ((assign x 8)))))
;; '() => '((z 5) (y 7) (x 8))

(define program46
  '(
    (decl x)
    (assign x 6)
    (decl y)
    (assign y 7)
    (decl z)
    (assign z 6)
    (if (and (gt y x) (lt z x))
        ((assign x 8)))))
;; '() => '((z 5) (y 7) (x 6))

(define program47
  '(
    (decl x)
    (assign x 6)
    (decl y)
    (assign y 5)
    (decl z)
    (assign z 7)
    (if (and (gt y x) (lt z x))
        ((assign x 8)))))
;; '() => '((z 7) (y 5) (x 6))

(define program49
  '(
    (decl x)
    (assign x 6)
    (decl y)
    (assign y 7)
    (if (not (gt y x))
        ((assign x 8)))))
;; '() => '((y 7) (x 6))

(define program50
  '(
    (decl x)
    (assign x 7)
    (decl y)
    (assign y 6)
    (if (not (gt y x))
        ((assign x 8)))))
;; '() => '((y 6) (x 8))

(define program51
  '(
    (decl x)
    (assign x 2)
    (decl y)
    (assign y 2)
    (assign x (+ x y))))
;; '() => '((y 2) (x 4))

(define program52
  '(
    (decl x)
    (assign x 2)
    (decl y)
    (assign y 2)
    (assign x (- x y))))
;; '() => '((y 2) (x 0))

(define program53
  '(
    (decl x)
    (assign x 3)
    (decl y)
    (assign y 2)
    (assign x (* x y))))
;; '() => '((y 2) (x 6))

(define program54
  '(
    (decl x)
    (assign x 2)
    (decl y)
    (assign y 2)
    (assign x (/ x y))))
;; '() => '((y 2) (x 1))

(define program55
  '(
    (decl x)
    (assign x 2)
    (decl y)
    (assign y 3)
    (assign x (+ (- y x) (* y y)))))
;; '() => '((y 3) (x 10))

(define program56
  '(
    (decl x)
    (assign x 3)
    (decl y)
    (assign y 2)
    (while (gt x 0)
           (
            (assign y (* y y))
            (assign x (- x 1))))))
;; '() => '((y 256) (x 0))

(define program57
  '(
    (decl x)
    (assign x 3)
    (decl y)
    (assign y 4)
    (if (gt y x)
        ((decl x)
         (assign x 4)))
    (assign x (+ x 1))))
;; '() => '((y 4) (x 4))

(define program58
  '(
    (decl x)
    (assign x 2)
    (decl y)
    (assign y 4)
    (while (gt y x)
           ((decl z)
            (assign z (+ x 1))
            (assign x (+ x 1))))))
;; '() => '((y 4) (x 4))