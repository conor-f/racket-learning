#lang racket

(provide to-rna)

(define (mapping) (hash #\G #\C #\C #\G #\T #\A #\A #\U))

(define
  (to-rna dna)
  (list->string (map (lambda (c) (hash-ref (mapping) c)) (string->list dna)))
)
