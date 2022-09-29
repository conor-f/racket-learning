#lang racket

;https://bugs.python.org/file4451/timsort.txt

(require "insertion-sort.rkt")

(provide count-run compute-minrun merge enumerate-runs)

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Handles the merging of runs. This replaces merge_lo and merge_hi.
(define (merge a b)
  (cond
    [(empty? a) b]
    [(empty? b) a]
    [(<= (first a) (first b)) (flatten (list (first a) (merge (rest a) b)))]
    [else (flatten (list (first b) (merge a (rest b))))]
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Enumerates all runs.
(define (enumerate-runs l [index 0])
  (cond
    [(empty? l) '()]
    [else
      (let
        ([next-run-len (count-run l)])
        (append
          (list (list next-run-len index))
          (enumerate-runs (list-tail l next-run-len) (+ index next-run-len))
        )
      )
    ]
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Decides if the list should be merged.
(define (should-merge l)
  (let
    (
      [runs-list (enumerate-runs l)]
      [a (third (reverse runs-list))]
      [b (second (reverse runs-list))]
      [c (first (reverse runs-list))]
    )
    (
      (or
        (<= (first a) (+ (first b) (first c)))
        (<= (first b) (first c))
      )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Weak Timsort (because I'm lazy and don't want to do the galloping now).
;(define (weak-timsort l)
;  (let
;    [minrun (compute-minrun l)]
;  )
;)
