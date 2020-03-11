#lang racket


;; Review the result for the following
;; and try to understand the computation

;; (sem program1 '((x 10) (y 5)))
(define program1 '(var (x 1) (+ x y)))

;; (sem program2 '((x 25)))
(define program2 '(+ (var (x 1) x) (var (y 2) (+ y x))))

;; Review the implementation and try to see its mapping
;; to the operational semantics discussed in class.
;; If there is a mismatch, identify.

(define (sem expr env)
  (cond
    [ (number? expr)   expr ]
    [ (symbol? expr)   (findvalue expr env) ]
    [ (arithop? (car expr))    (semarith expr env) ]
    [ (equal? (car expr) 'var)   (sem (cadr (cdr expr)) (consenv (cadr expr) env)) ]
    [ (condop? (car (car expr)))  (if (semcond (car expr) env)
                                     (sem (cadr expr) env)
                                     (sem (cadr (cdr expr)) env)) ]
    ))

(define (consenv varexprpair env)
  (cons (list (car varexprpair) (sem (cadr varexprpair) env)) env))


; ((x 5) (y 20) (z 20) (x 6))
;; findvalue pre-condition: variable v's mapping
;;                          is present in the environment
(define (findvalue v env)
  (if (equal? (car (car env)) v)
      (cadr (car env))
      (findvalue v (cdr env))))

(define (condop? el)
  (or (equal? el 'or)
      (equal? el 'and)
      (equal? el 'lt)
      (equal? el 'gt)
      ;; ...
      ))

(define (arithop? el)
  (or (equal? el '+)
      (equal? el '-)
      ;...
      ))

(define (semarith expr env)
  (cond
    [ (equal? (car expr) '+)  (+ (sem (cadr expr) env)
                                 (sem (cadr (cdr expr)) env)) ]
    [ (equal? (car expr) '-)  (- (sem (cadr expr) env)
                                 (sem (cadr (cdr expr)) env)) ]
    ;...
    ))

(define (semcond expr env)
  (cond
    [ (equal? (car expr) 'gt)  (> (sem (cadr expr) env)
                                  (sem (cadr (cdr expr)) env)) ]
    [ (equal? (car expr) 'lt)  (< (sem (cadr expr) env)
                                  (sem (cadr (cdr expr)) env)) ]
    [ (equal? (car expr) 'or)  (or (semcond (cadr expr) env)
                                  (semcond (cadr (cdr expr)) env)) ]
    ;; ...
    ))