#lang racket

(require racket/trace)

#|

|#

(define (length lst)
    (if (null? lst)
        0
        (+ 1 (length (cdr lst)))
    )
)

(trace length)