#lang racket

(require racket/file)
(require racket/set)


; Return a list of pairs like (cons direction distance)
(define (parse-input filename)
  (map
    (lambda (line)
      (let
        ([split_line (string-split line)])
        (cons
          (first split_line)
          (string->number (second split_line))
        )
      )
    ) (file->lines filename)) 
)

; Makes instructions all have distance 1
; e.g. (list (cons "U" 3)) -> (list (cons "U" 1) (cons "U" 1) (cons "U" 1))
(define (simplify-instructions instructions)
  (if
    (empty? instructions)
    '()
    (let
      (
       [current_instruction (first instructions)]
       [current_direction (car (first instructions))]
       [current_distance (cdr (first instructions))]
       [rest_of_instructions (rest instructions)]
       )
      (if
        (eq? current_distance 1)
        (append (list current_instruction) (simplify-instructions rest_of_instructions))
        (append
          (list (cons current_direction 1))
          (simplify-instructions
            (append
              (list (cons current_direction (- current_distance 1)))
              rest_of_instructions
              )
            )
          )
        )
      )
  )
)

; Update the head by 1 position in a direction
(define (move-head direction head_position)
  (let
    (
      [x (car head_position)]
      [y (cdr head_position)]
    )
    (cond
      [(string=? direction "U") (cons x (+ y 1))]
      [(string=? direction "D") (cons x (- y 1))]
      [(string=? direction "L") (cons (- x 1) y)]
      [(string=? direction "R") (cons (+ x 1) y)]
    )
  )
)

(define (is-overlapping? a b)
  (let
    (
      [a_x (car a)]
      [a_y (cdr a)]
      [b_x (car b)]
      [b_y (cdr b)]
    )
    (and
      (eq? a_x b_x)
      (eq? a_y b_y)
    ) 
  ) 
)

(define (is-touching-horizontally? a b)
  (let
    (
      [a_x (car a)]
      [a_y (cdr a)]
      [b_x (car b)]
      [b_y (cdr b)]
    )
    (and
      (eq? a_x b_x)
      (eq? a_y b_y)
    ) 
  ) 
)

(define (is-touching? a b)
  (let
    (
      [a_x (car a)]
      [a_y (cdr a)]
      [b_x (car b)]
      [b_y (cdr b)]
      [min_x (- (car a) 1)]
      [max_x (+ (car a) 1)]
      [min_y (- (cdr a) 1)]
      [max_y (+ (cdr a) 1)]
    )
    (and
      (>= b_x min_x)
      (<= b_x max_x)
      (>= b_y min_y)
      (<= b_y max_y)
    ) 
  )
)

(define (move-tail tail_position head_position direction)
  (let
    (
      [head_x (car head_position)]
      [head_y (cdr head_position)]
      [tail_x (car tail_position)]
      [tail_y (cdr tail_position)]
    )
    (cond
      ; Same column or row:
      [
        (or (eq? head_x tail_x) (eq? head_y tail_y))
        (cons
          (/ (+ head_x tail_x) 2)
          (/ (+ head_y tail_y) 2)
        )
      ]
      ; Diagonally separated means we can just set the tail position to be one
      ; step in the opposite direction that the head just moved:
      [else
        (cond
          [(string=? direction "U") (cons head_x (- head_y 1))]
          [(string=? direction "D") (cons head_x (+ head_y 1))]
          [(string=? direction "L") (cons (+ head_x 1) head_y)]
          [(string=? direction "R") (cons (- head_x 1) head_y)]
        )
      ]
    )
  )
)

(define (get-new-tail-position tail_position new_head_position direction)
  (if (is-touching? tail_position new_head_position)
    tail_position
    (move-tail tail_position new_head_position direction)
  )
)

; instruction is (cons "U" 3)
; positions are (x y)
; Return is (cons (cons head_x head_y) (cons tail_x tail_y))
(define (run-instruction instruction head_position tail_position)
  (let
    (
      [direction (car instruction)]
      [distance (cdr instruction)]
    )
    (if (eq? distance 0)
      (cons head_position tail_position)
      (let
        (
          [old_head_position head_position]
          [new_head_position (move-head direction head_position)]
        )
        (run-instruction
          (cons direction (- distance 1))
          new_head_position
          (get-new-tail-position tail_position new_head_position direction)
        )
      )
    )
  )
)

; Part 1
(define (get-tail-path)
  (let
    (
      [tail_positions (list (cons 0 0))]
      [tail_position (cons 0 0)]
      [head_position (cons 0 0)]
      ; [instructions (parse-input "9.test")]
      [instructions (parse-input "9.real")]
    )
    (map
      (lambda (instruction)
        (let
          ([result (run-instruction instruction head_position tail_position)])
          (set! head_position (car result))
          (set! tail_position (cdr result))
          (set! tail_positions (append tail_positions (list tail_position)))
        )
      )
      (simplify-instructions instructions)
    )
    tail_positions
  )
)

(length (remove-duplicates (get-tail-path)))
