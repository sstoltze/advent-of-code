#lang racket/base
(provide make-tokenizer)

(require brag/support)

(define-lex-abbrev skipped-terms (:or "by" "x" "y" "="))
(define-lex-abbrev reserved-terms (:or "rotate row" "rotate column" "rect"))

(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define screen-lexer
  (lexer-srcloc
   ["\n" (token 'NEWLINE lexeme)]
   [whitespace (token lexeme #:skip? #t)]
   [skipped-terms (token lexeme lexeme #:skip? #t)]
   [reserved-terms (token lexeme lexeme)]
   [digits (token 'INTEGER (string->number lexeme))]))

(define (make-tokenizer ip [path #f])
  (port-count-lines! ip)
  (lexer-file-path path)
  (define (next-token)
    (screen-lexer ip))
  next-token)
