#lang racket

(require (for-syntax racket/base
                     syntax/parse)
         syntax/parse/define)

(module blah racket
  (require syntax/parse/define)

  #;(define-syntax-class signature
    (pattern))
  (define-syntax (define-contract-out stx)
    (syntax-parse stx
      [(_ (f:id params:id ...)
          signature
          body ...+)
       #:declare signature (expr/c #'contract?)
       #'(begin
           (define (f params ...)
             body ...)
           (provide
            (contract-out
             [f signature.c])))]))

  (define-contract-out (wrong-bigger-string x)
    666
    (number->string (add1 x)))

  (define-contract-out (bigger-string x)
    (-> number? string?)
    (number->string (add1 x))))

(require 'blah
         rackunit)

(check-equal? "667" (bigger-string 666))

(check-exn #rx"^bigger-string: contract violation*"
           (thunk (bigger-string #f)))