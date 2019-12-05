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


(define (string->intcode-program s)
  (list->vector (map string->number
                     (string-split s #px"[\\s,]+"))))

(define (day2-1-input)
  (string->intcode-program (file->string "../input/day2-1.txt")))

(define (intcode-op f)
  (lambda (program ptr)
    (define (deref p) (vector-ref program p))
    (vector-set! program (deref (+ ptr 3))
                 (f (deref (deref (+ ptr 1)))
                    (deref (deref (+ ptr 2)))))
    (values program (+ ptr 4))))

(define intcode-1 (intcode-op +))
(define intcode-2 (intcode-op *))
(define intcode-99 (lambda (program ptr) (values eof ptr)))

(define (intcode-machine ops)
  (lambda (input)
    (let loop ([program input]
               [pointer 0])
      (define opcode (vector-ref program pointer))
      (define proc (hash-ref ops opcode))
      (define-values (prog ptr) (proc program pointer))
      (if (eof-object? prog)
          program
          (loop prog ptr)))))

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
