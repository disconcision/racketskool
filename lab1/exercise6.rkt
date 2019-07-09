#lang racket

(require syntax/parse/define
         rackunit)

(define-syntax (condd stx)
  (syntax-parse stx
    [(_ [else expr])
     #'expr]
    [(_ [condition expr] more ...)
     #'(if condition
           expr
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

(check-equal?
 (ffilter even? (range 10))
 (filter even? (range 10)))