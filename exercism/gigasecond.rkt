#lang racket

(provide add-gigasecond)

(require racket/date)

(define (add-gigasecond t)
  (seconds->date (+ (date->seconds t) (expt 10 9)))
)
