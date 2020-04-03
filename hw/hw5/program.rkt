#lang racket

(provide (all-defined-out))

(define p0
  `(
    (fundecl (f (x)) (
		       (assign y (+ x 1))
		       )
	      )
    (decl y)
    (call (f (0)) 1)
    )
  )

(define p1
  `(
    (fundecl (f (x))( (assign y (+ x 1))))
    (decl y)
    (decl z)
    (assign z f)
    (call (z (0)) 1)
    )
  )

(define p2
  `(
    (decl y)
    (decl z)
    (assign z f)
    (call (z (0)) 1)
    )
  )

(define p3
`(
  (decl x)
  (fundecl (f (z)) (
    (assign x (+ z 1)))
  )
  (fundecl (g (y)) (
    (decl x)
    (call (f (y)) 0))
    )

  (call (g (2)) 1)
  )
)


(define p4
`(
(decl x)
(fundecl (f (z)) (
(assign x (+ z 1))
)
)
(fundecl (g (y)) (
(decl x)
(call (f (y)) 1)
)
)
(call (g (2)) 1)
))


(define p5
`(
	(decl x)
	(fundecl (f (z)) (
		(assign x (+ z 1)))
	)
	(fundecl (g (y)) ( (call (f (y)) 1)))
	(fundecl (h (w)) ( (decl x)(call (g (w)) 0)))
	(call (h (10)) 0)
))
