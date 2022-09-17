#lang racket

(provide collatz)

(define (collatz n)
  (if (or (<= n 0) (not (eq? n (floor n)) )) (exn:fail) 
  (if (eq? n 1)
      0
      (if (even? n)
          (+ 1 (collatz (/ n 2)))
          (+ 1 (collatz (+ (* 3 n) 1)))
       )
  ))
)
