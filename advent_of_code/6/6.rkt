#lang racket

(require racket/file)


(define (parse-input filename)
  (string->list (first (file->lines filename)))
)

(define (is-start-of-packet-marker l)
  (= (length l) (length (remove-duplicates l)))
)

; Pushes e onto q and removes the last element of q.
; e.g. (push-and-pop (list 1 2 3) 4) -> (list 2 3 4)
(define (push-and-pop q e)
  (append (drop q 1) (list e))
)

(define (find-sopm all_data window)
  (if (is-start-of-packet-marker window)
    (length window)
    (+
      1
      (find-sopm
        (drop all_data 1)
        (push-and-pop window (first all_data))
      )
    )
  )
)

(let
  ([input (parse-input "6.real")])
  (find-sopm
    (drop input 4)
    (take input 4)
  )
)

(let
  ([input (parse-input "6.real")])
  (find-sopm
    (drop input 14)
    (take input 14)
  )
)
