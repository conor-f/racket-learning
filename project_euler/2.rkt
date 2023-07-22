#lang racket

(require racket/file)

(define (next-fib l)
  (apply + (take-right l 2))
)

(define (get-fibs-below x [current-fibs '(0 1)]) 
  (cond
    [(>= (next-fib current-fibs) x) current-fibs]
    [else (get-fibs-below
            x
            (append current-fibs (list (next-fib current-fibs)) )
          )
    ]
  )
)

(apply + (filter
  even?
  (get-fibs-below 4000000)
)) 
