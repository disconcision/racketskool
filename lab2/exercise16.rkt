#lang racket


(require syntax/parse/define
         (for-syntax syntax/parse
                     racket/stxparam)
         racket/stxparam
         syntax/macro-testing
         rackunit)
#;
(define-syntax (define-return stx)
  (syntax-parse stx
   
    [(_ (f:id as:id ...)
        body:expr ...)
     #:with return (datum->syntax #'f 'return)
     #'(define (f as ...)
         (let/ec return
           body ...))]))

(define-syntax-parameter return
  (λ (stx) (raise-syntax-error 'return "no!!!!!")))

(define-syntax (define-return2 stx)
  (syntax-parse stx
    [(_ (f:id as:id ...)
        body:expr ...)
     #'(define (f as ...)
         (let/ec k
           (syntax-parameterize ([return (make-rename-transformer #'k)])
             body ...)
           ) 
         )
     ]))


(define-return2 (f a)
  (if a
      (return 6)
      (return 7))
  (println "666"))