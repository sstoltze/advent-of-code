#lang racket/base
(require racket/file
         racket/string
         racket/set
         racket/match
         racket/list
         racket/vector
         racket/function)

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
  (file->intcode-program "../input/day2-1.txt"))

(define (file->intcode-program f)
  (string->intcode-program (file->string f)))

(define (string->intcode-program s)
  (list->vector (map string->number
                     (string-split s #px"[\\s,]+"))))

(define (intcode-parameter program ptr modes)
  (lambda (m [immediate #f])
    (if immediate
        (vector-ref program (+ ptr m))
        (case (hash-ref modes m 0)
          [(1)  (vector-ref program (+ ptr m))]
          [else (vector-ref program (vector-ref program (+ ptr m)))]))))

(define (intcode-op f)
  (lambda (program ptr [modes (make-hash)])
    (define deref (intcode-parameter program ptr modes))
    (vector-set! program (deref 3 #t)
                 (f (deref 1)
                    (deref 2)))
    (values program (+ ptr 4))))

(define intcode-1 (intcode-op +))
(define intcode-2 (intcode-op *))
(define (intcode-3 program ptr [modes (make-hash)])
  (define deref (intcode-parameter program ptr modes))
  (vector-set! program (deref 1 #t)
               (string->number (string-trim (read-line))))
  (values program (+ ptr 2)))
(define (intcode-4 program ptr [modes (make-hash)])
  (define deref (intcode-parameter program ptr modes))
  (println (deref 1))
  (values program (+ ptr 2)))
(define (intcode-5 program ptr [modes (make-hash)])
  (define deref (intcode-parameter program ptr modes))
  (if (not (zero? (deref 1)))
      (values program (deref 2))
      (values program (+ ptr 3))))
(define (intcode-6 program ptr [modes (make-hash)])
  (define deref (intcode-parameter program ptr modes))
  (if (zero? (deref 1))
      (values program (deref 2))
      (values program (+ ptr 3))))
(define (intcode-7 program ptr [modes (make-hash)])
  (define deref (intcode-parameter program ptr modes))
  (vector-set! program (deref 3 #t)
               (if (< (deref 1)
                      (deref 2))
                   1
                   0))
  (values program (+ ptr 4)))
(define (intcode-8 program ptr [modes (make-hash)])
  (define deref (intcode-parameter program ptr modes))
  (vector-set! program (deref 3 #t)
               (if (= (deref 1)
                      (deref 2))
                   1
                   0))
  (values program (+ ptr 4)))
(define (intcode-99 program ptr [modes (make-hash)])
  (values eof ptr))

(define (intcode-machine ops [debug #f])
  (lambda (input)
    (let loop ([program input]
               [pointer 0])
      (when debug
        (printf "Pointer: ~A~%" pointer)
        (printf "Program: ~A~%" program))
      (define opcode (vector-ref program pointer))
      (define op     (modulo opcode 100))
      (define modes  (for/hash ([i (in-naturals 1)]
                                [c (in-list (reverse (string->list (number->string (floor (/ opcode 100))))))])
                       (values i (string->number (string c)))))
      (when debug
        (printf "Opcode: ~A~%Op: ~A~%Modes: ~A~%" opcode op modes))
      (define proc (hash-ref ops op))
      (when debug
        (printf "Proc: ~A~%~%" proc))
      (define-values (prog ptr) (proc program pointer modes))
      (if (eof-object? prog)
          program
          (loop prog ptr)))))

(define (intcode-5-6-7-8-test)
  (define machine (intcode-machine (make-hash (list (cons 1 intcode-1)
                                                    (cons 2 intcode-2)
                                                    (cons 3 intcode-3)
                                                    (cons 4 intcode-4)
                                                    (cons 5 intcode-5)
                                                    (cons 6 intcode-6)
                                                    (cons 7 intcode-7)
                                                    (cons 8 intcode-8)
                                                    (cons 99 intcode-99)))))
  (println "Checks whether input is equal to 8.")
  (define prog (string->intcode-program "3,9,8,9,10,9,4,9,99,-1,8"))
  (machine prog)
  (println "Checks whether input is less than 8.")
  (define prog-prime (string->intcode-program "3,9,7,9,10,9,4,9,99,-1,8"))
  (machine prog-prime)
  (println "Checks whether input is equal to 8.")
  (define prog2 (string->intcode-program "3,3,1108,-1,8,3,4,3,99"))
  (machine prog2)
  (println "Checks whether input is less than 8.")
  (define prog2-prime (string->intcode-program "3,3,1107,-1,8,3,4,3,99"))
  (machine prog2-prime)
  (println "Ouput 999 if input is less than 8, 1000 if it is equal and 1001 if it is larger.")
  (define prog3 (string->intcode-program "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"))
  (machine prog3)
  (println "Outputs 0 when input is zero:")
  (define prog4 (string->intcode-program "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"))
  (machine prog4)
  (println "Outputs 0 when input is zero:")
  (define prog5 (string->intcode-program "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"))
  (machine prog5)
  (println "Multiply numbers and write the result.")
  (define prog6 (string->intcode-program "1002,4,2,4,52,4,4,4,99"))
  (machine prog6)
  (println "Outputs the given input:")
  (define prog7 (string->intcode-program "3,0,4,0,99"))
  (machine prog7)
  )

(define day2-intcode-machine (intcode-machine (make-hash (list (cons 1 intcode-1)
                                                               (cons 2 intcode-2)
                                                               (cons 99 intcode-99)))))

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

(struct point (x y) #:transparent)

(define (manhattan-dist p1 p2)
  (+ (abs (- (point-x p1) (point-x p2)))
     (abs (- (point-y p1) (point-y p2)))))

(define (origin-dist p)
  (manhattan-dist p (point 0 0)))

(define (day3-input)
  (map (compose (lambda (s) (string-split s ",")) string-trim) (file->lines "../input/day3-1.txt")))

(define day3-test-input
  (map (lambda (s) (string-split s ",")) '("R8,U5,L5,D3" "U7,R6,D4,L4")))

(define (move-point p dir len)
  (match-define (point x y) p)
  (case dir
    [(#\U) (point x (+ y len))]
    [(#\D) (point x (- y len))]
    [(#\L) (point (- x len) y)]
    [(#\R) (point (+ x len) y)]))

(define (add-wire p dir len )
  (for/list ([i (in-range len)])
    (move-point p dir (add1 i))))

(define (wire->points wire)
  (for/fold ([points (set)]
             [p (point 0 0)]
             #:result points)
            ([w (in-list wire)])
    (define direction (string-ref w 0))
    (define length (string->number (substring w 1)))
    (define new-points (set-union points (list->set (add-wire p direction length))))
    (values new-points
            (move-point p direction length))))

(define (intersect-wires w1 w2)
  (define first-wire-points (wire->points w1))
  (for/fold ([intersection (set)]
             [p (point 0 0)]
             #:result intersection)
            ([w w2])
    (define direction (string-ref w 0))
    (define length (string->number (substring w 1)))
    (values (set-union intersection
                       (set-intersect first-wire-points
                                      (list->set (add-wire p direction length))))
            (move-point p direction length))))

(define (wire-steps wire target)
  (for/fold ([result #f]
             [length 0]
             [p (point 0 0)]
             #:result result)
            ([w wire])
    (define direction (string-ref w 0))
    (define wire-length (string->number (substring w 1)))
    (if (not result)
        (values
         (cond
           [(index-of (add-wire p direction wire-length) target) => (lambda (i) (+ length (add1 i)))]
           [else #f])
         (+ length wire-length)
         (move-point p direction wire-length))
        (values result length p))))

(define (day3 input)
  (match-define (list w1 w2) input)
  (define intersections (intersect-wires w1 w2))
  (values (argmin origin-dist (set->list intersections))
          (apply min (map (lambda (p) (+ (wire-steps w1 p)
                                         (wire-steps w2 p)))
                          (set->list intersections)))))

(define day5-intcode-machine (intcode-machine (make-hash (list (cons 1 intcode-1)
                                                               (cons 2 intcode-2)
                                                               (cons 3 intcode-3)
                                                               (cons 4 intcode-4)
                                                               (cons 5 intcode-5)
                                                               (cons 6 intcode-6)
                                                               (cons 7 intcode-7)
                                                               (cons 8 intcode-8)
                                                               (cons 99 intcode-99)))
                                              ))

(define (day5-1-input)
  (file->intcode-program "../input/day5-1.txt"))

(define (day5 input)
  (println "5-1: Enter 1 when prompted.")
  (day5-intcode-machine (vector-copy input))
  (println "5-2: Enter 5 when prompted.")
  (day5-intcode-machine (vector-copy input)))
