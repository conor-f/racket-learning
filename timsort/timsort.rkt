#lang racket

(provide count-run compute-minrun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Returns the run count of l in a particular order.
(define (count-run-generic l op)
  (cond
    [(empty? l) 0]
    [(eq? (length l) 1) 1]
    [(op (first l) (second l)) (+ 1 (count-run-generic (rest l) op))]
    [else 1]
  )
)

; Returns the number of elements in the next run.
(define (count-run l)
  (cond
    [(empty? l) 0]
    [(eq? (length l) 1) 1]
    [(> (first l) (second l)) (count-run-generic l >)]
    [else (count-run-generic l <=)]
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Computes minrun for a list.
(define (compute-minrun l)
  (if (< (length l) 64)
    (length l)
    (let ([bin-n (number->string (length l) 2)])
      (+
        (string->number (substring bin-n 0 6) 2)
        (if (string-contains? (substring bin-n 6) "1") 1 0)
      )
    )
  )
)
