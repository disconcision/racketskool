#lang racket

(module blah racket
  (require syntax/parse/define)
  
  (define-syntax (define-contract-out stx)
    (syntax-parse stx
      [(_ (f params ...)
          signature
          body ...)
       #'(begin
           (define (f params ...)
             body ...)
           (provide
            (contract-out
             [f signature])))]))

  (define-contract-out (bigger-string x)
    (-> number? string?)
    (number->string (add1 x))))

(require 'blah
         rackunit)

(check-equal? "667" (bigger-string 666))

(check-exn #rx"^bigger-string: contract violation*"
           (thunk (bigger-string #f)))
