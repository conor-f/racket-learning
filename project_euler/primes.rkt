#lang racket

(define (list-contains? l e)
  (cond
    [(empty? l) #f]
    [(eq? (first l) e) #t]
    [else (list-contains? (rest l) e)]
  )
)

(define (factors-of n)
  (filter
    (lambda (x) (eq? (remainder n x) 0))
    (build-list n add1)
  )
)

(define (build-list-between a b)
  (if (eq? a b)
    (list a)
    (append (list a) (build-list-between (add1 a) b))
  )
)

(define (limited-factors-of n)
  (filter
    (lambda (x) (eq? (remainder n x) 0))
    (build-list-between 2 (floor (sqrt n)))
  )
)

(define (stupid-is-prime? n)
  (eq? (length (factors-of n)) 2)
)

(define (less-stupid-is-prime? n)
  (empty? (limited-factors-of n))
)

; (define (prime? x)
;   (cond
;     [(eq? x 0) #f]
;     [(eq? x 1) #f]
;     [(eq? x 2) #t]
;     [else (list-contains? (primes-below))]
;   )
;   (list x (+ x 1))
; )
; 
; (define (primes-below n)
;   (build-list n values)
; )
; 
; (build-list 15 prime?)

(less-stupid-is-prime? 15485863)
