#lang racket

(provide hamming-distance)

(define (hamming-distance a b)
  (let
      ([la (string->list a)]
       [lb (string->list b)]
      )
    
    (if (not (equal? (length la) (length lb)))
      (exn:fail)
      (if (empty? la)
          0
          (if (eq? (first la) (first lb))
              (hamming-distance (list->string (rest la)) (list->string (rest lb)))
              (+ 1 (hamming-distance (list->string (rest la)) (list->string (rest lb))))
          )
      )
    )
  )
)
