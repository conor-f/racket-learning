#lang racket

(provide response-for)

(define (is-question? s)
  (string-suffix? s "?")
)

(define (is-yell? s)
  (and (string=? (string-upcase s) s) (regexp-match #rx".*[a-zA-Z].*" s))
)

(define (is-plain-question? s)
  (and (not (is-yell? s)) (is-question? s))
)

(define (is-plain-yell? s)
  (and (is-yell? s) (not (is-question? s)))
)

(define (is-yell-question? s)
  (and (is-yell? s) (is-question? s))
)

(define (is-address? s)
  (= (string-length (string-trim s)) 0)
)

(define (response-for s)
  (cond
    [(is-address? s) "Fine. Be that way!"]
    [(is-plain-question? s) "Sure."]
    [(is-plain-yell? s) "Whoa, chill out!"]
    [(is-yell-question? s) "Calm down, I know what I'm doing!"]
    [else "Whatever."]
  )
)
