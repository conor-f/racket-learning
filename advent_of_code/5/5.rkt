#lang racket

(require racket/file)

; Input:
;
;     [D]
; [N] [C]
; [Z] [M] [P]
;  1   2   3
; 
; move 1 from 2 to 1
; move 3 from 1 to 3
; move 2 from 2 to 1
; move 1 from 1 to 2
;
; Taking advantage of the data format to make an easier input parsing problem,
; we assume that each crate name is one char long and there are < 10 stack.
;
; We should represent the crates as a list of lists of chars:
; (list (list) (list \#Z \#N) (list \#M \#C \#D) (list \#P))
;
; And the moves as a pair of a number and a pair of two numbers:
; (cons 1 (cons 2 1))
; (cons 3 (cons 1 3))
; ...

(define (get-pair-from-string s)
  (cons
    (string->number (first (string-split s "-")))
    (string->number (second (string-split s "-")))
  )
)

; Given a list of strings, return a list of lists of strings, split wherever
; there's a new line.
;
; e.g. (split-on-empty-line (list "asdf" "" "jkl;")) -> (list (list "asdf") (list jkl;"))
(define (split-on-empty-lines lines)
  (let 
    ([split-index (index-of lines "")])
    (if (eq? split-index #f)
      (list lines)
      (append
        (list (take lines split-index)) 
        (split-on-empty-lines (drop lines (+ split-index 1)))
      )
    )
  )
)

; Given stack inputs and a value i, return the value at the top of the stack.
(define (get-stack-value input i)
  (let
    (
      [index (+ 1 (* 4 (- i 1)))]
      [top_row (first input)]
    )
    (if
      (or
        (eq? i 0)
        (>= index (string-length top_row))
        (char=? (string-ref top_row index) #\  )
      )
      #f
      (string-ref top_row index)
    )
  )
)

(define (parse-stack-from-input input i)
  (let
    (
      [input-height (length input)]
      [stack-i-value (get-stack-value input i)]
    )
    (if
      (or
        (eq? input-height 1)
        (eq? i 0)
      )
      (list)
      (if
        (eq? stack-i-value #f)
        (parse-stack-from-input (drop input 1) i)
        (append
          (list stack-i-value)
          (parse-stack-from-input (drop input 1) i)
        )
      )
    )
  )
)

(define (parse-stacks-from-input input)
  (build-list
    10
    (lambda (i)
      (parse-stack-from-input input i)
    )
  )
)

; Returns a pair of pairs representing the move.
; e.g. (parse-move-from-line "move 3 from 1 to 2")
; (cons 3 (cons 1 2))
(define (parse-move-from-line line)
  (let
    ([split-line (string-split line)])
    (cons
      (string->number (second split-line))
      (cons
        (string->number (fourth split-line))
        (string->number (sixth split-line))
      )
    )
  )
)

(define (parse-moves-from-input raw_moves)
  (map parse-move-from-line raw_moves)
)

; Takes a filename and returns parsed input:
(define (parse-input filename)
  (let
    ([input (split-on-empty-lines (file->lines filename))])
    (cons
      (parse-stacks-from-input (first input))
      (parse-moves-from-input (second input))
    )
  )
)

; Runs a move:
(define (run-move move stacks)
  ; If no crates to move, just return stacks.
  (if (eq? (car move) 0)
    (append stacks)
    (run-move
      ; Create a new move with one less crate to move:
      (cons
        (- (car move) 1)
        (cdr move)
      )
      ; And update the current stacks
      (let
        (
          [new_source_stack (drop (list-ref stacks (car (cdr move))) 1)]
          [new_dest_stack (append
                            (list (first (list-ref stacks (car (cdr move)))))
                            (list-ref stacks (cdr (cdr move)))
                          )]
        )
        (build-list
          (length stacks)
          (lambda (i)
            (cond
              ; Replace source stack with updated version:
              [(eq? i (car (cdr move))) new_source_stack]
              ; Replace dest stack with updated version:
              [(eq? i (cdr (cdr move))) new_dest_stack]
              [else (list-ref stacks i)]
            )
          )
        )
      )
    )
  )
)

(define (get-stack-heads stacks)
  (list->string (map first (filter (lambda (l) (not (empty? l))) stacks)))
)

;(get-stack-heads (foldl
;  (lambda
;    (move stacks)
;    (run-move move stacks)
;  )
;  ; Initial stacks:
;  (first (parse-input "5.real"))
;  ; Moves:
;  (cdr (parse-input "5.real"))
;))


; Part 2:
; Runs a move:
(define (run-move-part-2 move stacks)
  (let
    (
     [new_source_stack (drop (list-ref stacks (car (cdr move))) (car move))]
     [new_dest_stack (append
                       (take (list-ref stacks (car (cdr move))) (car move))
                       (list-ref stacks (cdr (cdr move)))
                       )]
    )
    (build-list
      (length stacks)
      (lambda (i)
        (cond
          ; Replace source stack with updated version:
          [(eq? i (car (cdr move))) new_source_stack]
          ; Replace dest stack with updated version:
          [(eq? i (cdr (cdr move))) new_dest_stack]
          [else (list-ref stacks i)]
        )
      )
    )
  )
)

(get-stack-heads (foldl
  (lambda
    (move stacks)
    (run-move-part-2 move stacks)
  )
  ; Initial stacks:
  (first (parse-input "5.real"))
  ; Moves:
  (cdr (parse-input "5.real"))
))

