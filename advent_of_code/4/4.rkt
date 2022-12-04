#lang racket

(require racket/file)

(define (get-pair-from-string s)
  (cons
    (string->number (first (string-split s "-")))
    (string->number (second (string-split s "-")))
  )
)

; Takes a string line and returns parsed:
; e.g. (parse-input-line "1-2,3-4") -> (cons (cons 1 2) (cons 3 4))
(define (parse-input-line line)
  (cons
    (get-pair-from-string (first (string-split line ",")))
    (get-pair-from-string (second (string-split line ",")))
  )
)

; Takes a filename and returns parsed input:
;
; file.txt:
;   1-2,3-4
;   2-5,5-6

; (parse-input "file.txt) ->
;   (list
;     (cons (cons 1 2) (cons 3 4))
;     (cons (cons 2 5) (cons 5 6))
;   )
(define (parse-input filename)
  (map parse-input-line (file->lines filename))
)

; Returns True if range a is contained by range b
(define (is-subrange a b)
  (and
    (>= (car a) (car b))
    (<= (cdr a) (cdr b))
  )
)

; Counts the number of subranges:
(define (count-subranges l)
  (map 
    (lambda ranges
      ; TODO Why are the ranges here within a list??
      ;(display "\nRanges:\n")
      ;(display ranges)

      (or
        (is-subrange (car (first ranges)) (cdr (first ranges)))
        (is-subrange (cdr (first ranges)) (car (first ranges)))
      )
    )
    l
  )
)

(foldl
  (lambda (val res) (+ res (if (eq? val #t) 1 0)))
  0
  (count-subranges (parse-input "4.real"))
)  


; Part 2:
; Returns True if range a overlaps at all with range b
(define (overlaps a b)
  (or
    (and
      (>= (car a) (car b))
      (<= (car a) (cdr b))
    )
    (and
      (>= (cdr a) (car b))
      (<= (cdr a) (cdr b))
    )
  )
)

(define (count-overlaps l)
  (map 
    (lambda ranges
      (or
        (overlaps (car (first ranges)) (cdr (first ranges)))
        (overlaps (cdr (first ranges)) (car (first ranges)))
      )
    )
    l
  )
)

(foldl
  (lambda (val res) (+ res (if (eq? val #t) 1 0)))
  0
  (count-overlaps (parse-input "4.real"))
)  
