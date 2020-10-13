#lang racket/base

(require racket/file
         racket/list
         racket/match
         racket/string
         racket/function)

(define (all? predicate lst)
  (if (empty? lst)
      #t
      (and (predicate (first lst))
           (all? predicate (rest lst)))))

(define (divide lst n)
  (cond [(empty? lst) (list)]
        [else (define-values (start rst) (split-at lst n))
              (cons start (divide rst n))]))

(define (triangle? x y z)
  (all? (match-lambda ((list a b c) (> (+ a b) c)))
        (permutations (list x y z))))

(define (day3-1-input)
  (map (lambda (line)
         (map string->number (string-split line)))
       (file->lines (string->path "../input/day3-1.txt"))))

(define (day3-2-input)
  ;; There is probably a better way...
  (define old-triangles (day3-1-input))
  (define (build-new-list list-of-triangles)
    (cond [(empty? list-of-triangles) (values (list) (list) (list))]
          [else (define-values (triangle rst) (split-at list-of-triangles 1))
                (match-define (list (list a b c)) triangle)
                (define-values (lst-a lst-b lst-c) (build-new-list rst))
                (values (cons a lst-a) (cons b lst-b) (cons c lst-c))]))
  (define-values (lst-a lst-b lst-c) (build-new-list old-triangles))
  (divide (append lst-a lst-b lst-c) 3))

(define (day3)
  (define possible-triangles (filter (curry apply triangle?) (day3-1-input)))
  (printf "Day 3-1: ~A possible triangles.~%" (length possible-triangles))
  (define possible-triangles-2 (filter (curry apply triangle?) (day3-2-input)))
  (printf "Day 3-2: ~A possible triangles.~%" (length possible-triangles-2)))
