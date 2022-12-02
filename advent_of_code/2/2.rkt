#lang racket

(require racket/file)

; Given a line of input (e.g. A Y) return the score for the choice you made.
(define (get-choice-score line)
  (let
    ([choice (string-ref line 2)])
    (cond
      [(or (char=? choice #\A) (char=? choice #\X)) 1]
      [(or (char=? choice #\B) (char=? choice #\Y)) 2]
      [(or (char=? choice #\C) (char=? choice #\Z)) 3]
    )
  )
)

; Given a line of input (e.g. A Y) return the score for the game result
(define (get-result-score line)
  (let
    (
      [opponent_move (string-ref line 0)]
      [my_move (cond
                 [(char=? (string-ref line 2) #\X) #\A]
                 [(char=? (string-ref line 2) #\Y) #\B]
                 [(char=? (string-ref line 2) #\Z) #\C]
                 [else (string-ref line 2)]
               )
      ]
    )
    (cond
      ; Draw:
      [(char=? opponent_move my_move) 3]
      ; Wins:
      [(and (char=? opponent_move #\A) (char=? my_move #\B)) 6]
      [(and (char=? opponent_move #\B) (char=? my_move #\C)) 6]
      [(and (char=? opponent_move #\C) (char=? my_move #\A)) 6]
      [else 0]
    )
  )
)

; Given a line of input (e.g. A Y) return the score.
(define (calculate-score line)
  (+ (get-choice-score line) (get-result-score line))
)

; Part 1:
;(apply + (map calculate-score (file->lines "2.test")))
;(apply + (map calculate-score (file->lines "2.real")))


; Part 2:
(define (get-new-move-line line)
  (let
    (
      [opponent_move (string-ref line 0)]
      [result (string-ref line 2)]
    )
    (cond
      ; Draw:
      [(char=? result #\Y) (string opponent_move #\  opponent_move)]
      ; Wins:
      [(and (char=? result #\Z) (char=? opponent_move #\A)) (string #\A #\ #\B)]
      [(and (char=? result #\Z) (char=? opponent_move #\B)) (string #\B #\ #\C)]
      [(and (char=? result #\Z) (char=? opponent_move #\C)) (string #\C #\ #\A)]
      ; Losses:
      [(and (char=? result #\X) (char=? opponent_move #\A)) (string #\A #\ #\C)]
      [(and (char=? result #\X) (char=? opponent_move #\B)) (string #\B #\ #\A)]
      [(and (char=? result #\X) (char=? opponent_move #\C)) (string #\C #\ #\B)]
      )
    )
)

;(apply + (map calculate-score (map get-new-move-line (file->lines "2.test"))))
(apply + (map calculate-score (map get-new-move-line (file->lines "2.real"))))
