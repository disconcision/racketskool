#lang racket

(require syntax/parse/define
         rackunit)

(require syntax/macro-testing)

(begin-for-syntax
  (define-splicing-syntax-class arg
    (pattern x:id)
    (pattern [x:id def-expr:expr])
    (pattern (~seq k:keyword x:id))
    (pattern (~seq k:keyword [x:id def-expr:expr])))
  (define-syntax-class my-formals
    (pattern xs:id)
    (pattern (x:arg ...))
    (pattern (x:arg ...+ . xs:id))))

(define-simple-macro (my-lambda fs:my-formals body:expr ...+)
  (lambda fs body ...))

(define-syntax (condd stx)
  (syntax-parse stx
    #:datum-literals (else)
    [(_ [(~literal else) e:expr] junk ...+)
     (raise-syntax-error 'condd
                         "else must be last"
                         stx
                         #'e)]
    [(_ [(~literal else) e:expr])
     #'e]
    [(_ [condition e:expr] more ...)
     #:declare condition (expr/c #'boolean?)
     #'(if condition
           e
           (condd more ...))]
    [(_ [#:def f expr] more ...)
     #'(let ([f expr])
         (condd more ...))]))


#;(check-exn #rx"blah" (thunk (convert-compile-time-error (begin
                                                            (define (ffilter ? l)
                                                              (condd [(empty? l) empty]
                                                                     [#:def f (first l)]
                                                                     [#:def fr (filter ? (rest l))]
                                                                     [(? f) (cons f fr)]
                                                                     [else fr]))
                                                            (ffilter even? (range 10))))))