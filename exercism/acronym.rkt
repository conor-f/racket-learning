#lang racket

(provide acronym)

(define (preprocess-string str)
  (string-split
    (string-replace (string-replace str "-" " ") "_" "")
  )
)

(define (acronym str)
  (apply string-append
         (map (lambda (word)
                (string-upcase (substring word 0 1)))
              (preprocess-string str)
              )
         )
)
