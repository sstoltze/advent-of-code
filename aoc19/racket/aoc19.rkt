#lang racket/base
(require racket/file
         racket/string
         racket/match
         racket/vector)

(define day1-input
  (file->list (string->path "../input/day1-1.txt")
              (lambda (port)
                (let ([in (read-line port 'any)])
                  (cond [(string? in) (string->number in)]
                        [(equal? in eof) eof]
                        [else 0])))))

(define (fuel mass)
  (max (- (floor (/ mass 3)) 2)
       0))

(define (day1-1 input)
  (for/sum ([mass input])
    (fuel mass)))

(define (total-fuel mass)
  (let ([f (fuel mass)])
    (if (<= f 0)
        0
        (+ f (total-fuel f)))))

(define (day1-2 input)
  (for/sum ([mass input])
    (total-fuel mass)))

(define (day2-1-input)
  (string->intcode-program (file->string "../input/day2-1.txt")))

(define (string->intcode-program s)
  (list->vector (map string->number
                     (string-split s #px"[\\s,]+"))))

(define (day2-intcode-machine program)
  (define (deref p) (vector-ref program p))
  (let loop ([ptr 0])
    (define opcode (deref ptr))
    (match opcode
      [(or 1 2)
       (vector-set! program (deref (+ ptr 3))
                    ((match opcode [1 +] [2 *])
                     (deref (deref (+ ptr 1)))
                     (deref (deref (+ ptr 2)))))
       (loop (+ ptr 4))]
      [99 program])))

(define (day2-1 input)
  (vector-set! input 1 12)
  (vector-set! input 2 2)
  (vector-ref (day2-intcode-machine input) 0))

(define (day2-2 input)
  (for/or ([noun (in-range 100)])
    (for/or ([verb (in-range 100)])
      (let ([v (vector-copy input)])
        (vector-set! v 1 noun)
        (vector-set! v 2 verb)
        (and (= (vector-ref (day2-intcode-machine v) 0)
                19690720)
             (+ (* noun 100) verb))))))
