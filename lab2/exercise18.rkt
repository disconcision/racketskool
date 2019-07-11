#lang racket


(require syntax/parse/define
         (for-syntax syntax/parse
                     racket/stxparam)
         racket/stxparam
         syntax/macro-testing
         rackunit)

(define-syntax-parameter it
  (lambda (stx) (raise-syntax-error 'return "Illegal outside extra-if" stx)))
(define-syntax (extra-if stx)
  (syntax-parse stx
    [(_ cond:expr
        then:expr
        else:expr)
     #'(let ((my-it cond))
         (syntax-parameterize
             ([it (make-rename-transformer #'my-it)])
           (if my-it
               then
               else)))]))

(define-syntax (extra-cond stx)
  (syntax-parse stx
    #:datum-literals (else)
    [(_) #'(void)]
    [(_ [(~literal else) e:expr] junk ...+)
     (raise-syntax-error 'extra-cond
                         "else must be last"
                         stx
                         #'e)]
    [(_ [(~literal else) e:expr])
     #'e]
    [(_ [condition e:expr] more ...)
     #:declare condition (expr/c #'boolean?)
     #'(extra-if condition
                 e
                 (extra-cond more ...))]
    #;[(_ [#:def f expr] more ...)
     #'(let ([f expr])
         (extra-cond more ...))]))

(define l (list 1 2 3))
(define ? even?)
(extra-cond
 [(empty? l) empty]
 #;[#:def f (first l)]
 #;[#:def fr (filter ? (rest l))]
 #;[else fr]
 [666 (println it)]
 #;[else 0])