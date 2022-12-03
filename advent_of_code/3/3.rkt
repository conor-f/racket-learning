#lang racket

(require racket/file)

(provide split-input)

; Given a character, convert it to its value.
(define (char->value c)
  (let
    ([all_chars (string->list "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")])
    (index-of all_chars c)
  )
)

; Given a string input, return a list of two lists of chars, halving the
; string.
; e.g. (split-input "abcd") -> (list (list #\a #\b) (list #\c #\d))
(define (split-input i)
  (let
    (
      [string_len (string-length i)]
      [split_index (/ (string-length i) 2)]
    )
    (list
      (string->list (substring i 0 split_index))
      (string->list (substring i split_index))
    )
  )
)

; Given two lists of chars, return the char that appears in both
(define (get-common-element l1 l2)
  (check-duplicates (append (remove-duplicates l1) (remove-duplicates l2)))
)

(define (get-result-for-line s)
  (let
    (
      [l1 (first (split-input s))]
      [l2 (second (split-input s))]
    )
    (char->value (get-common-element l1 l2))
  )
)

; Part 1:
;(map get-result-for-line (file->lines "3.test"))
;(apply + (map get-result-for-line (file->lines "3.test")))
;(apply + (map get-result-for-line (file->lines "3.real")))


; Part 2:
; Given two lists of chars, return the char that appears in both
(define (get-common-elements l1 l2)
  (let
    ([dup (check-duplicates (append (remove-duplicates l1) (remove-duplicates l2)))])
    (if (and (boolean? dup) (not dup))
      (list)
      (append (list dup) (get-common-elements (remove* (list dup) l1) (remove* (list dup) l2)))
    ) 
  )
)

; Given a list l, split it into sublists of size n
; e.g. (chunks (list 1 2 3 4 5) 2) -> (list (list 1 2) (list 3 4) (list 5))
(define (chunks l n)
  (if (<= (length l) n)
    (list l)
    (append (list (take l n)) (chunks (drop l n) n))
  )
)

; Given a list of lists, return a list of all elements found in all lists.
; e.g. (get-intersection-of-lists (list (list 1 2 3 4) (list 2 3 4 5) (list 3 4 5 6)) -> (list 3 4)
(define (get-intersection-of-lists l)
  (cond
    [(= (length l) 1) (first l)]
    [(= (length l) 2) (get-common-elements (first l) (second l))]
    [else
      (get-intersection-of-lists
        (append
          (list (get-common-elements (first l) (second l)))
          (drop l 2)
        )
      )
    ]
  )
)

;(get-common-elements (list 1 2 3 4) (list 2 3 4 5))
;(chunks (list 1 2 3 4 5) 2)
;(get-intersection-of-lists (list (list 1 2 3 4) (list 2 3 4 5) (list 3 4 5 6)))
(apply
  +
  (map
    char->value
    (flatten
      (map
        get-intersection-of-lists
        (chunks
          (map string->list (file->lines "3.real"))
          3
        )
      )
    )
  )
)
