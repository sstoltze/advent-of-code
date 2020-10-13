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

(define (day4-room str)
  (match-define (list names ... id-checksum) (string-split str "-"))
  (match-define (list _ sector-id checksum) (regexp-match #rx"([0-9]+)\\[([a-zA-Z]+)\\]" id-checksum))
  (list (string-join names "-") (string->number sector-id) checksum))

(define (day4-input)
  (map day4-room
       (file->lines (string->path "../input/day4-1.txt"))))

(define (valid-room? lst)
  (match-define (list name sector-id checksum) lst)
  (define letters (sort (group-by identity (sort (filter (lambda (c) (not (char=? c #\-)))
                                                         (string->list name))
                                                 char<?))
                        >
                        #:key length))
  (define most-common (list->string (map first (take letters 5))))
  (if (string=? most-common checksum)
      sector-id
      #f))

(define (room-id room)
  (or (valid-room? room)
      0))

(define (shift-char int char)
  (if (char=? char #\-)
      #\Space
      (integer->char
       (+ (modulo (+ (- (char->integer char)
                        (char->integer #\a))
                     int)
                  (- (add1 (char->integer #\z))
                     (char->integer #\a)))
          (char->integer #\a)))))

(define (real-room-name room)
  (match-define (list name sector-id _) room)
  (list->string (map (curry shift-char sector-id)
                     (string->list name))))

(define (northpole-room? room)
  (match-define (list _ sector-id _) room)
  (define real-name (real-room-name room))
  (if (regexp-match? #rx"north" real-name)
      (format "~A: ~A" real-name sector-id)
      #f))

(define (day4)
  (define real-rooms (filter valid-room? (day4-input)))
  (define sum-of-ids (apply + (map room-id real-rooms)))
  (printf "Day 4-1: ~A~%" sum-of-ids)
  (printf "Day 4-2:~%")
  (define northpole-rooms (filter identity (map northpole-room? real-rooms)))
  (map println northpole-rooms))
