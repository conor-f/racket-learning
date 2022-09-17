#lang racket

(provide armstrong-number?)

; Returns a list of each element of l raised to the power of n:
(define (exponentify-list l n)
  (if (empty? l)
    '(1)
    (map (lambda (i) (expt i n)) l)
  )
)

; Returns the sum of a list:
(define (sum-list l)
  (if (empty? l)
    0
    (+ (first l) (sum-list (rest l)))
  )
)

; Convert number to list of digits:
(define (num->digits n)
  (map (lambda (c) (- (char->integer c) 48)) (string->list (number->string n)))
)

(define (armstrong-number? n)
  (let
    ([n-list (num->digits n)]
     [n-length (length (num->digits n))])
    (eq? n (sum-list (exponentify-list n-list n-length)))
  )
)
