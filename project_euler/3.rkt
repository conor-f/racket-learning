#lang racket

(require racket/file)

(define (is-factor x y)
  (eq? (remainder y x) 0)
)

(define (is-prime x)
  #t
)

(define (get-prime-factors n)
  (filter
    (lambda (x)
      (and
        (is-factor x n)
        (is-prime x)
      )
    )
    (build-list n add1)
  )
)

(get-prime-factors 12)
(get-prime-factors 9)
(get-prime-factors 100)
(get-prime-factors 13195)



; (define (get-fibs-below x [current-fibs '(0 1)]) 
;   (cond
;     [(>= (next-fib current-fibs) x) current-fibs]
;     [else (get-fibs-below
;             x
;             (append current-fibs (list (next-fib current-fibs)) )
;           )
;     ]
;   )
; )
; 
; (apply + (filter
;   even?
;   (get-fibs-below 4000000)
; )) 
