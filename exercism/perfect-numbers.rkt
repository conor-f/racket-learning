#lang racket

(provide classify)

;(define (sequence n)
;  (if (eq? n 1) (list 1) (append (list n) (sequence (- n 1))))
;)

(define (sequence n)
  (map (lambda (i) (+ i 1)) (build-list n values))
)
  
(define (factor? n a)
  (= (modulo n a) 0)
)

;(define (factors n)
;  (filter positive? (map (lambda (i)
;         (if (factor? n i) i -1)
;       )
;       (sequence (- n 1))
;   )
;))

;(define (factors n)
;  (filter (lambda (i) (factor? n i)) (sequence (- n 1)))
;)

(define (sqrtfactors n)
  (filter (lambda (i) (factor? n i)) (sequence (+ (exact-floor (sqrt n)) 1)))
)

(define (factors n)
  (remove n (remove-duplicates (append (sqrtfactors n) (map (lambda (i) (/ n i) ) (sqrtfactors n)))))
)

(define (sum-list l)
  (if (null? l) 0 (+ (car l) (sum-list (cdr l))))
)

(define (aliquot-sum n)
  (sum-list (factors n))
)

(define (classify n)
  (cond
    [(> (aliquot-sum n) n) 'abundant]
    [(eq? (aliquot-sum n) n) 'perfect]
    [(< (aliquot-sum n) n) 'deficient]
  )
)
