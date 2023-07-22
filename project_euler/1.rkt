#lang racket

(require racket/file)

(apply + (filter
  (lambda (x)
    (or
      (eq?
        (remainder x 3)
        0
      )
      (eq?
        (remainder x 5)
        0
      )
    )
  )
  (build-list 1000 values)
)) 
