#lang racket

(provide isogram?)

(define (isogram? s)
  (let
      ([s-list (string->list (string-upcase (string-replace (string-replace s "-" "") " " ""))) ])
      (equal? (length s-list) (length (set->list (list->set s-list))))
  )
)
