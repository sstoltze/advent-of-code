#lang racket/base

(require syntax/strip-context
         (for-syntax syntax/parse
                     racket/base))

(module+ reader
  (provide read-syntax))

(define (read-syntax path port)
  (define wire-datums
    (for/list ([wire-str (in-lines port)])
      (read (open-input-string (format "(wire ~a)" wire-str)))))
  (strip-context
   #`(module wires-mod wires/main
       #,@wire-datums)))
(provide #%module-begin #%datum #%app #%top #%top-interaction)

(define-syntax (wire stx)
  (syntax-parse stx
    #:datum-literals (->)
    [(_ arg          -> id) #'(define/display (id) (val arg))]
    [(_ op arg       -> id) #'(wire (op (val arg))             -> id)]
    [(_ arg1 op arg2 -> id) #'(wire (op (val arg1) (val arg2)) -> id)]
    [else #'(void)]))
(provide wire)

(define-syntax (define/display stx)
  (syntax-parse stx
    [(_ (id) body) #'(begin
                       (define (id) body)
                       (module+ main
                         (displayln (format "~a: ~a" 'id (id)))))]))

(define val
  (let ([val-cache (make-hash)])
    (λ (num-or-wire)
      (if (number? num-or-wire)
          num-or-wire
          (hash-ref! val-cache num-or-wire num-or-wire)))))

(define (mod16-bit x) (modulo x 65536))
(define-syntax (define-16bit stx)
  (syntax-parse stx
    [(_ id proc-id) #'(define id (compose1 mod16-bit proc-id))]))

(define-16bit AND bitwise-and)
(define-16bit OR bitwise-ior)
(define-16bit NOT bitwise-not)
(define-16bit LSHIFT arithmetic-shift)
(define (RSHIFT x y) (LSHIFT x (- y)))
(provide AND OR NOT LSHIFT RSHIFT)
