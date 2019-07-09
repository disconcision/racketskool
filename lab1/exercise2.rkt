#lang racket

(require syntax/parse/define
         rackunit)

(define-syntax (fseq2 stx)
  (syntax-parse stx
    [(_ expr f g)
     #'(g (f expr))]))

(check-equal? "8" (fseq2 7 add1 number->string))