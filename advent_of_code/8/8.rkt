#lang racket

(require racket/file)


(define (char->number c)
  (- (char->integer c) 48)
)

(define (parse-input filename)
  (map (lambda (line) (map char->number (string->list line))) (file->lines filename)) 
)

; Returns the (x, y) value of l where (0, 0) is the top left
(define (2d-index l x y)
  (list-ref (list-ref l y) x)
)

(define (get-col l n)
  (map (lambda (row) (list-ref row n)) l)
)

(define (get-row l n)
  (list-ref l n)
)

(define (is-on-edge? l i)
  (or
    (eq? i 0)
    (eq? i (- (length l) 1))
  )
)

(define (list-max l)
  (if (empty? l) '() (first (sort l >)))
)

; Returns #t if l contains a value greater than x
(define (contains-greater? l x)
  (<= x (list-max l))
)

(define (is-1d-visible? l i)
  (if (is-on-edge? l i)
    #t
    (let
      (
        [height (list-ref l i)]
        [list-before (take l i)]
        [list-after (drop l (+ i 1))]
      )
      ; An index is invisible iff there's a tree of greater or equal height on
      ; both sides of it:
      (or
        (not (contains-greater? list-before height))
        (not (contains-greater? list-after height))
      )
    )
  )
)

; Return True if (x, y) is visible 
(define (is-visible? l x y)
  (or
    (is-1d-visible? (get-row l y) x)
    (is-1d-visible? (get-col l x) y)
  )
)

(let
  (
    [forest (parse-input "8.real")]
    [height (length (parse-input "8.real"))]
    [width (length (first (parse-input "8.real")))]
    [running_total 0]
  )
  (for-each (lambda (x)
              (for-each (lambda (y)
                          (set! running_total
                            (+ running_total (if
                                               (is-visible? forest x y)
                                               1
                                               0
                                               ))
                            ))

                        (build-list height values)
                        ))
    (build-list width values)
  )
  running_total
)

; Part 2:
(define (count-smaller l x)
  (cond
    [(empty? l) 0]
    [(>= (first l) x) 1]
    [else (+ 1 (count-smaller (drop l 1) x))]
  )
)

(define (get-upwards-scenic-score l x y)
  (count-smaller
    (reverse (take (get-col l x) y))
    (2d-index l x y)
  )
)

(define (get-downwards-scenic-score l x y)
  (count-smaller
    (drop (get-col l x) (+ 1 y))
    (2d-index l x y)
  )
)

(define (get-leftwards-scenic-score l x y)
  (count-smaller
    (reverse (take (get-row l y) x))
    (2d-index l x y)
  )
)

(define (get-rightwards-scenic-score l x y)
  (count-smaller
    (drop (get-row l y) (+ 1 x))
    (2d-index l x y)
  )
)

(define (get-scenic-score l x y)
  (*
    (get-upwards-scenic-score l x y)
    (get-downwards-scenic-score l x y)
    (get-leftwards-scenic-score l x y)
    (get-rightwards-scenic-score l x y)
  )
)


(let
  (
    [forest (parse-input "8.real")]
    [height (length (parse-input "8.real"))]
    [width (length (first (parse-input "8.real")))]
    [max_scenic_score 0]
  )
  (for-each (lambda (x)
              (for-each (lambda (y)
                          (let
                            ([scenic_score (get-scenic-score forest x y)])
                            (if (> scenic_score max_scenic_score)
                              (set! max_scenic_score scenic_score)
                              (set! max_scenic_score max_scenic_score)
                            )
                          ))
                        (build-list height values)
                        ))
    (build-list width values)
  )
  max_scenic_score
)
