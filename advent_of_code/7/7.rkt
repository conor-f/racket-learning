#lang racket

(require racket/file)


(define (get-obj-from-line line current_path)
  (list
    (string-append current_path (second (string-split line)))
    (string->number (first (string-split line)))
  )
)

(define (is-command-line? line)
  (string-prefix? line "$")
)

; Made this a new macro as it was really annoying -_-
(define (go-up-dir current_path)
  (let
    ([new_path (string-join
                 (drop-right (string-split current_path "/") 1) 
                 "/"
                 #:before-first "/"
                 #:after-last "/"
                 )])
    (if (string=? new_path "//") "/" new_path)
  )
)

; Given a command line, return the new path after that command
(define (get-new-path-from-command-line line current_path)
  (cond
    [(string=? line "$ ls") current_path]
    [(string=? line "$ cd /") "/"]
    [(string=? line "$ cd ..") (go-up-dir current_path)]
    [else (string-append current_path (last (string-split line)) "/")]
    )
  )

(define (is-dir-line? line)
  (string-prefix? line "dir")
)

; Returns a value to store and a new path
(define (process-line line current_path)
  (cond
    [(is-command-line? line) (list '()  (get-new-path-from-command-line line current_path))]
    [(is-dir-line? line) (list '() current_path)]
    [else (list (get-obj-from-line line current_path) current_path)]
  )
)


(define (parse-input filename)
  (let
    (
      [current_path ""]
      [current_result '()]
    )
    (filter (lambda (x) (not (empty? x)))
            (map (lambda (line)
        (set! current_result (process-line line current_path))
        (set! current_path (second current_result))
        (first current_result)
        )
                 (file->lines filename))
      
    )
  )
)

; Given a path, return if it is a dir or not
(define (is-dir path)
  (string-suffix? path "/")
)


; Given a string path, remove everything after the trailing "/"
; e.g. (strip-filename "/a/b/c.txt") -> "/a/b/"
(define (strip-filename path)
  (let
    ([split_path (string-split path "/")])
    (cond
      [(empty? split_path) "/"]
      [(eq? (length split_path) 1) "/"]
      [else (string-join (drop-right split_path 1)"/" #:before-first "/" #:after-last "/")] 
    )
  )
)

; Given a string path, return a list of all dirs it's made up of.
; e.g. (split-to-dirs "/a/b/c/d") -> (list "/" "/a/" "/a/b/" "/a/b/c/")
(define (split-to-dirs path)
  (if (string=? path "/")
    (list)
    (append (list (strip-filename path)) (split-to-dirs (strip-filename path)))
  )
)


; Given file sizes as a list of lists, return the size of all dirs.
; e.g. (list (list "/b.txt" 1) (list "/c.txt" 2) (list "/a/aa.txt" 3) (list
; "/a/aaa" 4)) -> (list (list "/" 10) (list "/a/" 7))
(define (get-dir-sizes file-list)
  (let
    ([dir-sizes (make-hash)])
    (begin
      (for-each
        (lambda (f)
          (let
            (
             [dirs (split-to-dirs (first f))]
             [size (second f)]
             )
            (for-each
              (lambda (dir)
                (hash-set!
                  dir-sizes
                  dir
                  (+ size (hash-ref dir-sizes dir 0)))
                )
              dirs
              )
            )
          )
        file-list
      )
      (hash->list dir-sizes)
    )
  )
)


; Return only dirs + sizes less than X
(define (get-small-dirs dirs-list)
  (filter
    (lambda (dir) (< (cdr dir) 100000))
    dirs-list
  )
)

; Part 2:
(define (get-smallest-size-over-threshold sorted-dirs threshold)
  (if (> (cdr (first sorted-dirs)) threshold)
    (first sorted-dirs)
    (get-smallest-size-over-threshold (list-tail sorted-dirs 1) threshold)
  )
)

(let
    ([sorted-dirs (sort
                    (get-dir-sizes (parse-input "7.real"))
                    (lambda (x y)
                      (< (cdr x) (cdr y))
                      )
                    )])
    (get-smallest-size-over-threshold
      sorted-dirs
      (- 30000000 (- 70000000 (cdr (last sorted-dirs))))
    )
  )
