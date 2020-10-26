#lang racket/base

(require racket/file
         racket/list
         racket/match
         racket/string
         racket/function
         racket/hash)

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

(define letter-map (hash))

(define (word->letters w)
  (for/fold ([res (hash)])
            ([(letter index) (in-indexed w)])
    (hash-set res index (list letter))))

(define (merge-letters l1 l2)
  (hash-union l1 l2
              #:combine append))

(define (extract-letter-by letters index sort-by)
  (caar (sort (group-by identity
                        (hash-ref letters index (list)))
              sort-by
              #:key length)))

(define (extract-word-by letters sort-by)
  (list->string (for/list ([i (in-list (sort (hash-keys letters)
                                             <))])
                  (extract-letter-by letters i sort-by))))

(define (words->letters words)
  (for/fold ([res (hash)])
            ([word (in-list words)])
    (merge-letters res (word->letters word))))

(define (day6)
  (define words (file->lines (string->path "../input/day6-1.txt")))
  (define letters (words->letters words))
  (define most-common-word (extract-word-by letters >))
  (printf "Day 6-1: ~A~%" most-common-word)
  (define least-common-word (extract-word-by letters <))
  (printf "Day 6-2: ~A~%" least-common-word))

(define (abba? s)
  (and (>= (string-length s) 4)
       (char=? (string-ref s 0)
               (string-ref s 3))
       (char=? (string-ref s 1)
               (string-ref s 2))
       (not (char=? (string-ref s 0)
                    (string-ref s 1)))))

(define (has-abba? s)
  (for/or ([i (in-range (string-length s))])
    (abba? (substring s i))))

(define (day7-1-input)
  (file->lines (string->path "../input/day7-1.txt")))

(define (hypernet-split input-ip)
  (define ip-list (if (list? input-ip)
                      input-ip
                      (string->list input-ip)))
  (define-values (supernet leftover) (splitf-at ip-list
                                          (lambda (c) (not (char=? c #\[)))))
  (cond
    [(empty? leftover) (values (list (list->string supernet))
                               null)]
    [else (define-values (hypernet leftover-ip) (splitf-at leftover
                                                           (lambda (c) (not (char=? c #\])))))
          (define-values (supernets hypernets) (hypernet-split (drop leftover-ip 1)))
          (values (cons (list->string supernet)
                        supernets)
                  (cons (list->string (drop hypernet 1))
                        hypernets))]))

(define (tls? ip)
  (define-values (supernets hypernets) (hypernet-split ip))
  (and (ormap has-abba? supernets)
       (not (ormap has-abba? hypernets))))

(define (aba? s)
  (and (>= (string-length s) 3)
       (char=? (string-ref s 0)
               (string-ref s 2))
       (substring s 0 3)))

(define (aba->bab aba)
  (string (string-ref aba 1)
          (string-ref aba 0)
          (string-ref aba 1)))

(define has-bab? string-contains?)

(define (all-aba s)
  (for/list [(i (in-range (string-length s)))
             #:when (aba? (substring s i))]
    (aba? (substring s i))))

(define (ssl? ip)
  (define-values (supernets hypernets) (hypernet-split ip))
  (define abas (append-map all-aba supernets))
  (for/or ([aba (in-list abas)])
    (ormap (lambda (h)
             (h . has-bab? . (aba->bab aba)))
           hypernets)))

(define (day7)
  (define ips (day7-1-input))
  (define has-tls (filter tls? ips))
  (printf "Day 7-1: ~A ips has TLS.~%" (length has-tls))
  (define has-ssl (filter ssl? ips))
  (printf "Day 7-2: ~A ips has SSL.~%" (length has-ssl)))
