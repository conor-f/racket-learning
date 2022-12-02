#lang racket

(require racket/file)

; Non-Rackety implementation:
; (let
;   (
;    [max_value 0]
;    [current_value 0]
;    [input (file->lines "1_real.inp")]
;   )
;   (for ([row input])
;     (let ([row_as_number (string->number row)])
;       (if (not (equal? row_as_number #f))
;         (begin
;           (set! current_value  (+ current_value row_as_number))
;           (when (> current_value max_value)
;             (set! max_value current_value)
;           )
;         )
;         (begin
;           (set! current_value 0)
;         )
;       )
;     )
;   )
; 
;   (display max_value)
; )


; Splits a list into a list of lists on empty elements:
; e.g.:
; (list 1 2 3 "" 4 5 6) -> (list (list 1 2 3) (list 4 5 6))
(define (split-list l)
  (if (member #f l)
    (let ([split_index (index-of l #f)])
      (append
        (list (take l split_index))
        (split-list (list-tail l (+ split_index 1)))
      )
    )
    (list l)
  )
)

(apply
  +
  (take
    (sort
      (map
        (lambda (l) (apply + l))
        ;(split-list (map string->number (file->lines "1.test")))
        (split-list (map string->number (file->lines "1_real.inp")))
      )
      >
    )
    3
  )
)
