#lang racket/base

(provide #%module-begin
         #%datum
         #%top
         #%top-interaction
         #%app
         (all-defined-out))

(require (for-syntax racket/base
                     syntax/parse)
         racket/vector
         racket/string)

(module+ reader
    (require syntax/strip-context
         "parser.rkt"
         "tokenizer.rkt")
    (provide read-syntax)
    (define (read-syntax path port)
      (define parse-tree (parse path (make-tokenizer port path)))
      (strip-context
       #`(module screen-module screen
           #,path
           #,parse-tree))))

(define-syntax (-> stx)
  (syntax-parse stx
    [(_ expr) #'expr]
    [(_ a (f args ...) exprs ...) #'(-> (f a args ...) exprs ...)]))

(define-syntax (->> stx)
  (syntax-parse stx
    [(_ expr) #'expr]
    [(_ a (f args ...) exprs ...) #'(->> (f args ... a) exprs ...)]))

(define-syntax (screen-program stx)
  (syntax-parse stx
    [(_ statements ...) #'(-> (init-screen 50 6)
                              statements ...
                              (screen-output))]))

(define (init-screen x y)
  (make-vector y (make-vector x)))

(define *screen-on* 1)

(define (pixel-on? p)
  (equal? p *screen-on*))

(define (format-pixel p)
  (format "~A" (if (pixel-on? p) "#" " ")))

(define (format-screen screen)
  (string-join (for/list ([line (in-vector screen)])
                 (->> line
                      (vector->list)
                      (map format-pixel)
                      ((lambda (l) (string-join l "")))))
               "\n"))

(define (println-screen screen)
  (printf "~A~%" (format-screen screen)))

(define (screen-ref screen x y)
  (vector-ref (vector-ref screen y) x))

(define (screen-set screen x y val)
  (define new-screen (for/vector ([line (in-vector screen)])
                       (vector-copy line)))
  (vector-set! (vector-ref new-screen y) x val)
  new-screen)

(define (rect screen x y)
  (for/fold ([s1 screen])
            ([i (in-range x)])
    (for/fold ([s2 s1])
              ([j (in-range y)])
      (screen-set s2 i j *screen-on*))))

(define (rotate-row screen y amount)
  (for/vector ([(line index) (in-indexed screen)])
    (if (= index y)
        (shift-right line amount)
        line)))

(define (shift-right row amount)
  (define len (vector-length row))
  (build-vector len (lambda (i) (vector-ref row (modulo (- i amount) len)))))

(define (rotate-column screen x amount)
  (define height (vector-length screen))
  (for/vector ([(line j) (in-indexed screen)])
    (define len (vector-length line))
    (build-vector len (lambda (i) (if (= i x)
                                      (screen-ref screen i (modulo (- j amount) height))
                                      (screen-ref screen i j))))))

(define (screen-count screen)
  (for/sum ([line (in-vector screen)])
    (vector-length (vector-filter pixel-on? line))))

(define (screen-output screen)
  (println-screen screen)
  (printf "~A pixels on.~%" (screen-count screen)))
