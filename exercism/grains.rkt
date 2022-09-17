#lang racket

(provide square total)

(define (square number)
  (if (= number 1)
      1
      (* 2 (square (- number 1)))
      )
)

(define
  (tot total)
  (cond
    [(= total 1) 1]
    [(= total 0) 0]
    [else (+ (square total) (tot (- total 1)))]
   )
)

(define (total)
  (tot 64))
