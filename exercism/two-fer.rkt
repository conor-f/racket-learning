#lang racket

(provide two-fer)

(define
  two-fer
  (lambda ([name "you"])
  (string-append
   (string-append "One for " name)
   ", one for me."
  )
))
