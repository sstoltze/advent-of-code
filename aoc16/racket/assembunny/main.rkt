#lang racket/base

(provide #%module-begin
         #%datum
         #%top
         #%top-interaction
         #%app
         (all-defined-out))

(require (for-syntax racket/base
                     syntax/parse)
         racket/hash
         racket/string)

(module+ reader
  (require syntax/strip-context
           "parser.rkt"
           "tokenizer.rkt")
  (provide read-syntax)
  (define (read-syntax path port)
    (define parse-tree (parse path (make-tokenizer port path)))
    (strip-context
     #`(module assembunny-module assembunny
         #,path
         #,parse-tree))))

(define-syntax (assembunny-program stx)
  (syntax-parse stx
    [(_ statements ...)
     #'(run-program (list statements ...))]))

(define-syntax (assembunny-line stx)
  (syntax-parse stx
    [(_ op) #'(lambda (ptr)
                (define inc-ptr op)
                (+ ptr inc-ptr))]))

(define (run-program prog)
  (define program-length (length prog))
  (let loop ([program prog]
             [pointer 0])
    (define out-of-bounds (>= pointer (sub1 program-length)))
    (define current-op (if out-of-bounds
                           0
                           (list-ref program pointer)))
    (if out-of-bounds
        (values program pointer)
        (loop program (current-op pointer)))))

(define registers (make-hash))

(define (cpy val register)
  (hash-set! registers register (value-of val))
  1)

(define (inc register)
  (hash-update! registers register add1 0)
  1)

(define (dec register)
  (hash-update! registers register sub1 0)
  1)

(define (jnz register relative)
  (if (= (value-of register) 0)
      1
      (add1 relative)))

(define (value-of n)
  (if (string? n)
      (hash-ref registers n 0)
      n))
