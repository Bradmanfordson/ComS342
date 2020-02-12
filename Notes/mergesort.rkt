#lang racket

(require racket/trace)

#| FUNCTIONS |#


#| MergeSort - divide and conquer sorting method

Base Case: 
Recursive Case: 

|#
(define (mergesort lst)
  (if (< (length lst) 2)
      lst
      (merge (mergesort (car (divide lst (/ (length lst) 2))))
	     (mergesort (cadr (divide lst (/ (length lst) 2))))
	     )
      )
)


#| Divide - Divide the list in half |#
(define (divide lst n)
  (if (< n 1)
      (list `() lst)
      (divide_low (car lst) (divide (cdr lst) (- n 1)))
      )
)

#| Helper for divide |#
(define (divide_low e pair)
  (list (cons e (car pair))
	(cadr pair)
	)
  )



#| NEW MORE EFFICIENT way to do mergesort |#

(define (new_mergesort lst)
  (if (< (length lst) 2)
      lst
      (new_merge (divide lst (/ (length lst) 2)))
      )
  )

(define (new_merge pair)
  (new_merge (new_mergesort (car pair))
	 (new_mergesort (cadr pair))
	 )
  )

#| Merge - Put 2 lists back together

Assuming both of these lists are sorted already

|#

(define (merge lst1 lst2)
  (if (null? lst1)
      lst2
      (if (null? lst2)
	  lst1 ; this and everything above is the base case
	  (if (< (cat lst1) (cat lst2)) ; This and below is the recursive case
	      (cons (car lst1) (merge (cdr lst1) lst2))
	      (cons (car lst2) (merge lst1 (cdr lst2)))
          )
      )
  )
)



#| TRACES |#
(trace mergesort)
(trace divide)
(trace divide_low)
(trace merge)
