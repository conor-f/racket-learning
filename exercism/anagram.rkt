#lang racket

(provide anagrams-for)

(define (list-equal? l1 l2)
  (cond
    [(not (eq? (length l1) (length l2))) #f]
    [(empty? l1) #t]
    [else (and (eq? (first l1) (first l2)) (list-equal? (rest l1) (rest l2)))]
  )
)

(define (is-anagram-old? l1 l2)
  (and (not (list-equal? l1 l2))
       (not (empty? (filter-map (lambda (i) (list-equal? l1 i)) (permutations l2))))
       )
  )

(define (are-hashsets-equal? h1 h2)
  (and (eq? (hash-count h1) (hash-count h2))
       (hash-map h1 (lambda (key value)
                      (and (hash-has-key? h2 key)
                           (eq? value (hash-ref h2 key))
                       )
                    )
       )
  )
)

(define (add-to-hashset hashset key)
  (hash-update hashset key (lambda (i) (+ i 1)) 0)
)

(define (create-hashset-from-word word-as-list)
  (if (empty? word-as-list)
      (hash)
      (
       add-to-hashset (create-hashset-from-word (rest word-as-list)) (char-downcase (first word-as-list))
      )
  )
)

(define (all? l)
  (if (boolean? l) l
      (foldl (lambda (a result) (and a result)) #t l)
  ))

(define (is-anagram? l1 l2)
  (and (not (list-equal? l1 l2))
       (all? (are-hashsets-equal? (create-hashset-from-word l1) (create-hashset-from-word l2)))
  )
)

(define (anagrams-for word l)
  (let
      (
       [wordl (string->list word)]
       [l (map (lambda (i) (string->list i)) l)]
       )
    (map list->string (filter (lambda (i) (is-anagram? wordl i)) l))
   )
)
