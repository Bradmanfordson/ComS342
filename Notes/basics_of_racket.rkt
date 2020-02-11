#lang racket

(require racket/trace)  ; Extremely useful for debugging


#| Racket Version

Return the Length of a list

Base Case:
    Return 0
Recursive Case:
    return (1 + length(tail lst))

Usage: (length lst)
Example: (length `(1 2 3)) --> 3

|#
(define (length lst)
  (if (null? lst)
      0  ; Base Case
      (+ 1 (length (cdr lst)))  ; Recursive Case
  )
)


#| Racket Version

Return a single list that is lst2 appended to lst1

Usage: (append lst1 lst2)
Example (append `(1 2 3) `(a b c)) --> `(1 2 3 a b c)

|#
(define (append lst1 lst2)
  (if (null? lst1)
      lst2  ; Base Case
      (cons (car lst1) (append (cdr lst1) lst2))  ; Recursive Case
  )
)


#| Lambda Version

Return a single list that is lst2 appended to lst1
lst1 + lst2 = `(lst1 lst2) = lst3

Usage: ((append_lambda lst1) lst2)
Example: ((append_lambda `(1 2 3)) `(a b c)) --> `(1 2 3 a b c)

|#
(define append_lambda
  (lambda (lst1)
    (lambda (lst2)
      (if (null? lst1)
          lst2  ; Base Case
          (cons (car lst1) ((append_lambda (cdr lst1)) lst2))  ; Recursive Case
      )
    )
  )
)


#| Racket Version

Use append_lambda to append the list `(1 2 3)

Usage: (append_lambda_123 lst)
Example: (append_lambda_123 `(a b c)) --> `(1 2 3 a b c)

|#
(define append_lambda_123 (append_lambda `(1 2 3 )))



#| Iterator

Example: (iterator (lambda (x) (+ x 1)) `(1 2 3)) --> `(2 3 4)
Example: (iterator even? `(1 2 3)) --> `(#f #t #f)

|#
(define (iterator func lst)
  (if (null? lst)
      lst
      (cons (func (car lst)) (iterator func (cdr lst)))
  )
)


#| Super Iterator

Example: return new list such that for element in list: element = element + 1

(super_iterator `() (lambda (x) (+ x 1)) cons `(1 2 3))
    base = `()
    func = (lambda (x) (+ x 1))
    compose = cons
    lst = `(1 2 3)

    RETURN = `(2 3 4)



Example: Return number of elements in the list

(super_iterator 0 (lambda (x) 1) (lambda (x y) (+ x y)) `(1 2 3))
    base = 0
    func = (lambda (x) 1)
    compose = (lambda (x y) (+ x y))
    lst = `(1 2 3)
    RETURN = 3



Example: How many even numbers are in list

(super_iterator 0 even? (lambda (x y) (if x (+ 1 y) y)) `(1 2 3)

    RETURN = 1

|#
(define (super_iterator base func compose lst)
  (if (null? lst)
      base
      (compose (func (car lst) (super_iterator base func compose (cdr lst))))
  )
)


#| Divide
Divide the input list l1 into two such that 
first list contains the first n elements of l1
second contains the rest of hte elements of l1
I.e. the output is a pair of lists 

Base Case: If N is 0: return an empty list
Recursive Case: Else: divide the rest of the list by the value of n

Divide `(2 a b c) 2
--> ((2 a) (b c))
Then where do we add the 1? We add it to the first list
BUT if we do the car stuff, we lose the second element in the list
So we need to include and pass the second element. Kind of weird.
If we could store variables, then we would have just done that.

|#
(define (divide lst n)
  (if (< n 1)  ; if n is 0
      (list `() lst)  ; Base case
      (list (cons (car lst) (car (divide (cdr lst) (- n 1))))
            (cadr (divide (cdr lst ) (- n 1)))  ; cadr = 2nd element
      )
  )    
)


; Do the same thing as divide but split up better and runs more efficiently.
(define (new_divide lst n )
  (if (< n 1)
      (list `() lst)
      (divide_low (car lst) (new_divide (cdr lst) (- n 1)))
  )
)


; With this function, we have just made divide from exponential to linear time. Super nice. Does the same as divide just better.
; This is also a good technique for storing variables since we're not allowed to explicitly use variables
(define (divide_low e pair)
  (list (cons e (car pair))
	(cadr pair))
)


#| Traces |#
(trace iterator)
(trace append_lambda_123)
(trace length)
(trace append)
(trace divide)
