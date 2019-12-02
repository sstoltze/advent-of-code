#lang racket/base
(require racket/list
         racket/function)

(define (group-same lst)
  (for/fold ([last-elem #f]
             [current-list (list)]
             [res (list)]
             #:result (reverse (cons current-list res)))
            ([e lst])
    (if (or (equal? e last-elem)
            (empty? current-list))
        (values e (cons e current-list) res)
        (values e (list e) (cons current-list res)))))

(define (look-say s)
  (for/fold ([res ""])
            ([char-list (group-same (string->list s))])
    (format "~A~A~A" res (length char-list) (first char-list))))

(define (day10)
  (define input "1321131112")
  (for/fold ([s input]
             #:result (string-length s))
            ([i (in-range 40)])
    (look-say s)))
