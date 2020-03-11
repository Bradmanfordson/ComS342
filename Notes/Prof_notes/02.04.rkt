#lang racket
(require racket/trace)

; comments single line

#|
   length of a list: typical recursion pattern
   length of empty list is 0 - base case
   length of non-empty list is 1 + length of rest/tail of list
|#
(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))
   )
  )

;; sum of the numbers in a list
;; sum of empty list is 0 - base case
;; sum of non-empty list is first element + sum of the rest of the list
(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))

;; add e to the end of the lst
;; (add2end 'a '(1 2 3 4)) 
(define (add2end e lst)
  (if (null? lst)
      (cons e '())
      (cons (car lst) (add2end e (cdr lst)))))

;; add2endl is the name of the lambda abstraction
;;          objective add the first lambda paramter to the end of
;;                    end of the second lambda parameter
;;                   (pre-condition second lambda parameter is a list)
;; ((add2endl 'a) '(1 2 3 4)) - review carefully the invocation pattern
;;                           and how it is different from add2end
(define add2endl
  (lambda (e)
    (lambda (lst)
      (if (null? lst)
          (cons e '())
          (cons (car lst) ((add2endl e) (cdr lst)))))))


;;  reverse a list
(define (reverse lst)
  (if (< (length lst) 2)
      lst
      (add2end (car lst) (reverse (cdr lst)))))


;(trace length) ;; allows you to review the recursion involving function length


;; (append '(1 2) '(a b c))
;; append empty list to a list returns the second list
;; append a non-empty list 1 to a list 2 returns the result of adding
;;        the first element of list 1 to the result of appending the rest of list1 to list2      
(define (append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (append (cdr lst1) lst2))))

;; Using lambda abstraction to do append
(define appendl
  (lambda (lst1)
    (lambda (lst2)
      (if (null? lst1)
          lst2
          (cons (car lst1) ((appendl (cdr lst1)) lst2))))))

;; partially instantiated lambda abstraction
(define appendl123 (appendl '(1 2 3)))


;; iterator with application
;; result is a new list where f is applied to
;;        every element of the old list.
(define (ite f lst)
  (if (null? lst)
      lst
      (cons (f (car lst)) (ite f (cdr lst)))))

;; superiterator
(define (superite base f compose lst)
  (if (null? lst)
      base
      (compose (f (car lst)) (superite base f compose (cdr lst)))))

#|
Convert the above into lambda abstraction
(define superite
  (lambda (base)
    (lambda (f)
      ...)))

(define superitecounter (superite 0))

|#


;; divide the input list l1 into two such that
;;    first list contains the first n elements of l1
;;    second contains the rest of the elements of l1
;; I.e., the output is a pair of lists
(define (divide lst n)
  (if (< n 1)
      (list '() lst)
      (list (cons (car lst) (car (divide (cdr lst) (- n 1)))) 
            (cadr (divide (cdr lst) (- n 1))))))

(trace divide) ;; see how many times the same divide is called.
               ;; think about why this is necessary in the above implementation.

  