#lang racket

(provide read-dict sorted-list-contains?)

; Remove special chars from a word and make all lower case.
(define (clean-word w)
  (string-downcase w)
  ; (string-replace (string-downcase w) "'" "")
)

; Given a file path f containing words, read it into a list.
(define (read-dict f)
  (sort
    (map
      (lambda (word)
        (clean-word word)
      )
      (call-with-input-file f
        (lambda (port)
          (port->lines port)
        )
      )
    )
    string<?
  )
)

; Given a sorted list l, returns #t if e is an element of l.
(define (sorted-list-contains? l e)
  (let
    (
     [pivot-index (quotient (length l) 2)]
     [pivot-val (list-ref l (quotient (length l) 2))]
    )
    (cond
      [(string=? pivot-val e) #t]
      [(eq? (length l) 1) #f]
      [(string<? pivot-val e) (sorted-list-contains? (list-tail l pivot-index) e)]
      [(string>? pivot-val e) (sorted-list-contains? (take l pivot-index) e)]
    )
  )
)

; Returns #t if w is a word that always remains a valid word if you remove a
; letter from the start/end given dictionary l.
(define (is-reducible-word? w l)
  (and
    (sorted-list-contains? l w)
    (if
      (eq? (string-length w) 1) #t
      (or
        (is-reducible-word? (substring w 0 (- (string-length w) 1)) l)
        (is-reducible-word? (substring w 1) l)
      )
    )
  )
)

; Given a sorted list l, return all reducible words.
(define (get-reducible-words l)
  (filter
    (lambda (w) (is-reducible-word? w l))
    l
  )
)

(get-reducible-words (read-dict "small_dict.txt"))
; Returns the subset of l that op evaluates to true for when applied to pivot.
;  (define (partition-list l pivot op)
;    (filter (lambda (x) (op x pivot)) l)
;  )
;  
;  ; Returns a subset of l that is >= pivot.
;  (define (higher-partition-list l pivot)
;    (partition-list l pivot >=)
;  )
;  
;  ; Returns a subset of l that is < pivot.
;  (define (lower-partition-list l pivot)
;    (partition-list l pivot <)
;  )
;  
;  ; Sorts a list in ascending order.
;  (define (quicksort l)
;    (cond
;      [(<= (length l) 1) l]
;      [else (let
;        ([pivot (first l)])
;        (append
;          (quicksort (lower-partition-list (rest l) pivot))
;          (list pivot)
;          (quicksort (higher-partition-list (rest l) pivot))
;        )
;      )]
;    )
;  )
