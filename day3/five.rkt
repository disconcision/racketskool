#lang racket

(require (for-syntax syntax/parse
                     racket/base))

(provide (rename-out [define:five define]))

(define-syntax (define:five stx)
  (define ls (syntax->list stx))
  (define name (list-ref ls 1))
  (define body (list-ref ls 2))
  (printf "assuming ~a is 5\n" (syntax->datum body))
  #`(define #,name 5)
  #;(syntax-parse stx
    [(_ id:id body:expr)
     #'(begin (printf "assuming ~a is 5\n" 'body)
            (define id 5))]))