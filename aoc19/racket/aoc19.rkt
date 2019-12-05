#lang racket/base
(require racket/file
         racket/string
         racket/set
         racket/match
         racket/list
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

(define (day3-1-input)
  (map (compose (lambda (s) (string-split s ",")) string-trim) (file->lines "../input/day3-1.txt")))

(define (add-wire direction length x y)
  (case direction
    [(#\U) (for/set ([i (in-range length)])
             (point x (+ y i 1)))]
    [(#\D) (for/set ([i (in-range length)])
             (point x (- y i 1)))]
    [(#\L) (for/set ([i (in-range length)])
             (point (- x i 1) y))]
    [(#\R) (for/set ([i (in-range length)])
             (point (+ x i 1) y))]))

(define (wire->points wire)
  (for/fold ([points (set)]
             [x 0]
             [y 0]
             #:result points)
            ([w (in-list wire)])
    (define direction (string-ref w 0))
    (define length (string->number (substring w 1)))
    (define new-points (set-union points (add-wire direction length x y)))
    (case direction
      [(#\U) (values new-points
                     x (+ y length))]
      [(#\D) (values new-points
                     x (- y length))]
      [(#\L) (values new-points
                     (- x length) y)]
      [(#\R) (values new-points
                     (+ x length) y)])))

(define (intersect-wires w1 w2)
  (define first-wire-points (wire->points w1))
  (for/fold ([intersection (set)]
             [x 0]
             [y 0]
             #:result intersection)
            ([w w2])
    (define direction (string-ref w 0))
    (define length (string->number (substring w 1)))
    (case direction
      [(#\U) (values (set-union
                      intersection
                      (set-intersect first-wire-points
                                     (add-wire direction length x y)))
                     x (+ y length))]
      [(#\D) (values (set-union
                      intersection
                      (set-intersect first-wire-points
                                     (add-wire direction length x y)))
                     x (- y length))]
      [(#\L) (values (set-union
                      intersection
                      (set-intersect first-wire-points
                                     (add-wire direction length x y)))
                     (- x length) y)]
      [(#\R) (values (set-union
                      intersection
                      (set-intersect first-wire-points
                                     (add-wire direction length x y)))
                     (+ x length) y)])))

(define (wire-steps wire point)
  (void))

(define (day3-1 input)
  (match-define (list w1 w2) input)
  (define intersections (intersect-wires w1 w2))
  (argmin origin-dist (set->list intersections)))
