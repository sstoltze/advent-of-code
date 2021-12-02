#lang racket/base
(require racket/file)
(require racket/list)
(require racket/function)

(define (day-1-input [f "../input/day1.txt"])
  (file->list (string->path f)))

(define (count-increases lst [key identity])
  (if (empty? lst)
      0
      (for/fold ([increases 0]
                 [current (key (first lst))]
                 #:result increases)
                ([next (in-list (rest lst))])
        (define val (key next))
        (if (> val current)
            (values (add1 increases) val)
            (values increases val)))))

(define (sliding-window lst n)
  (if (< (length lst) n)
      empty
      (for/fold ([result (list (take lst n))]
                 [current (take lst n)]
                 #:result (reverse result))
                ([next (in-list (drop lst n))])
        (define new-value (append (rest current) (list next)))
        (values (cons new-value result)
                new-value))))

(define (sum lst)
  (for/sum ([i (in-list lst)])
    i))

(define (day-1 [input (day-1-input)])
  (define depth-increases (count-increases input))
  (printf "Found ~A increases in depth.~%" depth-increases)
  (define sliding-increases (count-increases (sliding-window input 3) sum))
  (printf "Found ~A increases in depth using sliding windows.~%" sliding-increases))
