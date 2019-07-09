#lang racket

(require syntax/parse/define
         rackunit)

(define-syntax (fseq2 stx)
  (syntax-parse stx
    [(_ expr) #'expr]
    [(_ expr fs ... f)
     #'(f (fseq2 expr fs ...))]))

(check-equal? 7 (fseq2 7 add1 number->string string->number sub1))