#lang racket

(provide leap-year?)

(define (leap-year? year)
  (cond
    [
     (eq? (modulo year 4) 0)
     (if (and (eq? (modulo year 100) 0) (not (eq? (modulo year 400) 0))) #f #t)
    ]
    [else #f]
  )
)
