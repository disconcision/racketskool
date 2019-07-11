#lang racket

(require syntax/parse/define
         syntax/macro-testing
         rackunit)

(define-syntax (condd stx)
  (syntax-parse stx
     #:datum-literals (else)
    [(_ [else e:expr] junk ...+)
     (raise-syntax-error 'condd
                         "else must be last"
                         stx
                         #'e)]
    [(_ [else e:expr])
     #'e]
    [(_ [condition e:expr] more ...)
     #:declare condition (expr/c #'boolean?)
     #'(if condition
           e
           (condd more ...))]
    [(_ [#:def f expr] more ...)
     #'(let ([f expr])
         (condd more ...))]))

(define (ffilter ? l)
  (condd [(empty? l) empty]
         [#:def f (first l)]
         [#:def fr (filter ? (rest l))]
         [(? f) (cons f fr)]
         [else fr]))

(define l (list 1 2 3))
(define ? even?)
#;(condd [(empty? l) empty]
         [#:def f (first l)]
         [#:def fr (filter ? (rest l))]
         [else fr]
         [(? f) (cons f fr)])

#;(check-exn #rx"blah" (thunk (convert-compile-time-error (begin
                                                          (define (ffilter ? l)
  (condd [(empty? l) empty]
         [#:def f (first l)]
         [#:def fr (filter ? (rest l))]
         [(? f) (cons f fr)]
         [else fr]))
                                                          (ffilter even? (range 10))))))