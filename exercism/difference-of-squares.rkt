#lang racket

(provide sum-of-squares square-of-sum difference)

(define (sum-of-squares n)
  (if (equal? n 1)
      1
      (+ (expt n 2) (sum-of-squares (- n 1)))
  )
)

(define (square-of-sum n)
  (expt (/ (* n (+ n 1)) 2) 2)
)

(define (difference n)
  (- (square-of-sum n) (sum-of-squares n))
)
