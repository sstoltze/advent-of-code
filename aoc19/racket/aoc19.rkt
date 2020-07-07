#lang racket/base
(require racket/file
         racket/string
         racket/set
         racket/match
         racket/list
         racket/vector
         racket/function
         racket/hash)

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
  (for/hash ([i (in-naturals)]
             [p (in-list (map string->number
                             (string-split s #px"[\\s,]+")))])
    (values i p)))

(define (intcode-parameter program ptr relative-pointer modes [debug #f])
  (lambda (m [write-mode #f])
    (define output (case (hash-ref modes m 0)
                     [(2)  (if write-mode
                               (+ (hash-ref program (+ ptr m) 0) relative-pointer)
                               (hash-ref program (+ (hash-ref program (+ ptr m) 0) relative-pointer) 0))]
                     [(1)  (hash-ref program (+ ptr m) 0)]
                     [else (if write-mode
                               (hash-ref program (+ ptr m) 0)
                               (hash-ref program (hash-ref program (+ ptr m) 0) 0))]))
    (when debug
      (printf "Deref ~S~A: ~S~%" m (if write-mode "w" "") output))
    output))

(define (intcode-op f)
  (lambda (program ptr relative-pointer [modes (make-hash)] [debug #f])
    (define deref (intcode-parameter program ptr relative-pointer modes debug))
    (hash-set! program (deref 3 #t)
                 (f (deref 1)
                    (deref 2)))
    (values program (+ ptr 4) relative-pointer)))

(define intcode-1 (intcode-op +))
(define intcode-2 (intcode-op *))
(define (intcode-3 program ptr relative-pointer [modes (make-hash)] [debug #f])
  (define deref (intcode-parameter program ptr relative-pointer modes debug))
  (define input (read-line))
  (when debug
    (printf "Read input: ~S~%" input))
  (hash-set! program (deref 1 #t)
             (string->number (string-trim input)))
  (values program (+ ptr 2) relative-pointer))
(define (intcode-4 program ptr relative-pointer [modes (make-hash)] [debug #f])
  (define deref (intcode-parameter program ptr relative-pointer modes debug))
  (println (deref 1))
  (values program (+ ptr 2) relative-pointer))
(define (intcode-5 program ptr relative-pointer [modes (make-hash)] [debug #f])
  (define deref (intcode-parameter program ptr relative-pointer modes debug))
  (if (not (zero? (deref 1)))
      (values program (deref 2) relative-pointer)
      (values program (+ ptr 3) relative-pointer)))
(define (intcode-6 program ptr relative-pointer [modes (make-hash)] [debug #f])
  (define deref (intcode-parameter program ptr relative-pointer modes debug))
  (if (zero? (deref 1))
      (values program (deref 2) relative-pointer)
      (values program (+ ptr 3) relative-pointer)))
(define (intcode-7 program ptr relative-pointer [modes (make-hash)] [debug #f])
  (define deref (intcode-parameter program ptr relative-pointer modes debug))
  (hash-set! program (deref 3 #t)
               (if (< (deref 1)
                      (deref 2))
                   1
                   0))
  (values program (+ ptr 4) relative-pointer))
(define (intcode-8 program ptr relative-pointer [modes (make-hash)] [debug #f])
  (define deref (intcode-parameter program ptr relative-pointer modes debug))
  (hash-set! program (deref 3 #t)
               (if (= (deref 1)
                      (deref 2))
                   1
                   0))
  (values program (+ ptr 4) relative-pointer))
(define (intcode-9 program ptr relative-pointer [modes (make-hash)] [debug #f])
  (define deref (intcode-parameter program ptr relative-pointer modes debug))
  (values program (+ ptr 2) (+ relative-pointer (deref 1))))
(define (intcode-99 program ptr relative-pointer [modes (make-hash)] [debug #f])
  (values eof ptr relative-pointer))

(define (intcode-machine ops [debug-all #f])
  (lambda (input [debug debug-all])
    (let loop ([program (hash-copy input)]
               [pointer 0]
               [relative-pointer 0])
      (when debug
        (printf "Pointer: ~A~%" pointer)
        (printf "Relative pointer: ~A~%" relative-pointer)
        (printf "Program: ~A~%" program))
      (define opcode (hash-ref program pointer))
      (define op     (modulo opcode 100))
      (define modes  (for/hash ([i (in-naturals 1)]
                                [c (in-list (reverse (string->list (number->string (floor (/ opcode 100))))))])
                       (values i (string->number (string c)))))
      (when debug
        (printf "Opcode: ~A~%Op: ~A~%Modes: ~A~%" opcode op modes))
      (define proc (hash-ref ops op))
      (when debug
        (printf "Proc: ~A~%" proc))
      (define-values (prog ptr rel) (proc program pointer relative-pointer modes debug))
      (printf "~%")
      (if (eof-object? prog)
          program
          (loop prog ptr rel)))))

(define intcode-interpreter (intcode-machine (make-hash (list (cons 1 intcode-1)
                                                              (cons 2 intcode-2)
                                                              (cons 3 intcode-3)
                                                              (cons 4 intcode-4)
                                                              (cons 5 intcode-5)
                                                              (cons 6 intcode-6)
                                                              (cons 7 intcode-7)
                                                              (cons 8 intcode-8)
                                                              (cons 9 intcode-9)
                                                              (cons 99 intcode-99)))
                                             ))

(define (intcode-test [debug #f])
  (define machine (intcode-machine (make-hash (list (cons 1 intcode-1)
                                                    (cons 2 intcode-2)
                                                    (cons 3 intcode-3)
                                                    (cons 4 intcode-4)
                                                    (cons 5 intcode-5)
                                                    (cons 6 intcode-6)
                                                    (cons 7 intcode-7)
                                                    (cons 8 intcode-8)
                                                    (cons 9 intcode-9)
                                                    (cons 99 intcode-99)))
                                   debug))
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
  (println "Takes no input, produces itself as output.")
  (define prog8 (string->intcode-program "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"))
  (machine prog8)
  (println "Should produce a 16 digit number.")
  (define prog9 (string->intcode-program "1102,34915192,34915192,7,4,7,99,0"))
  (machine prog9)
  (println "Should print 1125899906842624.")
  (define prog10 (string->intcode-program "104,1125899906842624,99"))
  (machine prog10)
  )

(define (day2-1 input)
  (hash-set! input 1 12)
  (hash-set! input 2 2)
  (hash-ref (intcode-interpreter input) 0))

(define (day2-2 input)
  (for/or ([noun (in-range 100)])
    (for/or ([verb (in-range 100)])
      (let ([v (hash-copy input)])
        (hash-set! v 1 noun)
        (hash-set! v 2 verb)
        (and (= (hash-ref (intcode-interpreter v) 0)
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

(define (day5-1-input)
  (file->intcode-program "../input/day5-1.txt"))

(define (day5 input)
  (println "5-1: Enter 1 when prompted.")
  (intcode-interpreter input)
  (println "5-2: Enter 5 when prompted.")
  (intcode-interpreter input))

(define (day4-1-input)
  (map string->number (string-split (string-trim (file->string "../input/day4-1.txt")) "-")))

(define (six-digit? n)
  (= (string-length (number->string n)) 6))

(define (equal-adjacent? n)
  (define ns (number->string n))
  (for/or ([i (in-range (sub1 (string-length ns)))])
    (define sub (substring ns i (+ i 2)))
    (char=? (string-ref sub 0) (string-ref sub 1))))

(define (equal-adjacent-2? n)
  (define ns (number->string n))
  (define ns-l (string-length ns))
  (for/or ([i (in-range (sub1 ns-l))])
    (define sub (substring ns i (+ i 2)))
    (and (char=? (string-ref sub 0) (string-ref sub 1)))))

(define (increasing-digits? n)
  (define ns (number->string n))
  (for/and ([i (in-range (sub1 (string-length ns)))])
    (define sub (substring ns i (+ i 2)))
    (char<=? (string-ref sub 0) (string-ref sub 1))))

(define (day4-1-test? n)
  (and (six-digit? n)
       (equal-adjacent? n)
       (increasing-digits? n)))

(define (day4-1-numbers low high)
  (for/list ([n (in-range low high)]
             #:when (day4-1-test? n))
    n))

(define (string-group-equals s)
  (for/fold ([result (list)]
             [current-list (list)]
             #:result (reverse (cons current-list result)))
            ([c (in-string s)])
    (if (or (empty? current-list)
            (char=? c (first current-list)))
        (values result                     (cons c current-list))
        (values (cons current-list result) (list c)))))

(define (two-equal-adjacent? n)
  (not (empty?
        (filter (lambda (x) (= x 2))
                (map length (string-group-equals (number->string n)))))))

(define (day4 input)
  (define day4-1-output (day4-1-numbers (first input) (second input)))
  (printf "Number of passwords: ~A~%" (length day4-1-output))
  (printf "Number of updated passwords: ~A~%" (length (filter two-equal-adjacent? day4-1-output))))

(define (day9-1-input)
  (file->intcode-program "../input/day9-input.txt"))

(define (day9 input)
  (println "Enter 1 to get first answer.")
  (intcode-interpreter input)
  (println "Enter 2 to get second answer.")
  (intcode-interpreter input)
  #t)

(define (string-ref-default l i default)
  (if (<= 0 i (- (string-length l) 1))
      (string-ref l i)
      default))

(define (map-ref m i j)
  (define lines (string-split m "\n"))
  (if (<= 0 j (- (length lines) 1))
      (string-ref-default (list-ref lines j) i #\.)
      #\.))

(define (intersection-point? m x y)
  (and (char=? (map-ref m x y) #\#)
       (char=? (map-ref m (+ x 1) y) #\#)
       (char=? (map-ref m (- x 1) y) #\#)
       (char=? (map-ref m x (+ y 1)) #\#)
       (char=? (map-ref m x (- y 1)) #\#)))

(define (map->intersection-points m)
  (define lines (string-split m "\n"))
  (flatten
   (for/list ([i (in-range (string-length (first lines)))])
     (for/list ([j (in-range (length lines))]
                #:when (intersection-point? m i j))
       (point i j)))))

(define (alignment-parameter p)
  (* (point-x p) (point-y p)))

(define (calibration-parameter points)
  (apply + (map alignment-parameter points)))

(define (day17-1-input)
  (file->intcode-program "../input/day17-1-input.txt"))

(define (list->ascii-code l)
  (define (symbol->ascii-character s)
    (string-join
     (map (compose number->string char->integer)
          (string->list s))
     ","))
  (define (list->ascii-line line)
    (string-append
     (string-join (map symbol->ascii-character line)
                  ",44,")
     ",10"))
  (string-join (map list->ascii-line l)
               ","))

(define (string->map s)
  (list->string
   (map (compose integer->char string->number)
        (string-split s "\n"))))

(define (day17 input)
  (define output (let ([s (open-output-string)])
                   (parameterize ([current-output-port s])
                     (intcode-interpreter input)
                     (get-output-string s))))
  (define camera-view (string->map output))
  (display camera-view)
  (define intersection-points (map->intersection-points camera-view))
  (printf "The calibration parameter is ~S.~%" (calibration-parameter intersection-points))
  (define alt-program (hash-set input 0 2))
  (define input-string (list->ascii-code (list
                                          (list "A")
                                          (list "L" "10" "R" "8" "L" "6" "R" "6" "L" "8" "L" "8")
                                          (list "L")
                                          (list "R")
                                          (list "n")))
    )
  (let ([s (open-output-string)])
    (with-handlers ([(const #t) (lambda (e)
                                  (display (string->map (get-output-string s))))])
      (parameterize ([current-input-port (open-input-string input-string)]
                     [current-output-port s])
        (intcode-interpreter alt-program)
        (display (string->map (get-output-string s))))))
  )

(define (day7-1-input)
  (file->intcode-program "../input/day7-1-input.txt"))

(define (run-amplifiers amplifier phases)
  (let loop ([input 0]
             [phases phases])
    (cond
      [(empty? phases) input]
      [else            (define input-string (format "~A\n~A\n" (first phases) input))
                       (define output-string (open-output-string))
                       (define output (parameterize ([current-input-port (open-input-string input-string)]
                                                     [current-output-port output-string])
                                        (intcode-interpreter amplifier)
                                        (string->number (string-trim (get-output-string output-string)))))
                       (loop output (rest phases))])))

(define (day7 input)
  (for/fold ([result 0])
            ([phase (permutations '(0 1 2 3 4))])
    (max result (run-amplifiers input phase))))
