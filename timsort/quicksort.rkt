#lang racket

(provide quicksort)

; Returns the subset of l that op evaluates to true for when applied to pivot.
(define (partition-list l pivot op)
  (filter (lambda (x) (op x pivot)) l)
)

; Returns a subset of l that is >= pivot.
(define (higher-partition-list l pivot)
  (partition-list l pivot >=)
)

; Returns a subset of l that is < pivot.
(define (lower-partition-list l pivot)
  (partition-list l pivot <)
)

; Sorts a list in ascending order.
(define (quicksort l)
  (cond
    [(<= (length l) 1) l]
    [else (let
      ([pivot (first l)])
      (append
        (quicksort (lower-partition-list (rest l) pivot))
        (list pivot)
        (quicksort (higher-partition-list (rest l) pivot))
      )
    )]
  )
)
