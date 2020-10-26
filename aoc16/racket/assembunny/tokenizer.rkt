#lang racket/base
(provide make-tokenizer)

(require brag/support)

(define-lex-abbrev reserved-terms (:or "cpy" "inc" "dec" "jnz"))

(define-lex-abbrev digits (:+ (char-set "-0123456789")))

(define assembunny-lexer
  (lexer-srcloc
   ["\n" (token 'NEWLINE lexeme)]
   [whitespace (token lexeme #:skip? #t)]
   [reserved-terms (token lexeme lexeme)]
   [digits (token 'INTEGER (string->number lexeme))]
   [alphabetic (token 'REGISTER lexeme)]))

(define (make-tokenizer ip [path #f])
  (port-count-lines! ip)
  (lexer-file-path path)
  (define (next-token)
    (assembunny-lexer ip))
  next-token)
