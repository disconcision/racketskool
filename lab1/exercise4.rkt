#lang racket

(require syntax/parse/define
         rackunit)

(define-syntax (simple-for/and stx)
  (syntax-parse stx
    [(_ ([elem-name seq] ...) computation)
     (with-syntax ([(list-name ...) (generate-temporaries #'(elem-name ...))]
                   [how-many (length (syntax->list #'(elem-name ...)))])
       #'(letrec ([iterate
                   (λ (list-name ...)
                     (cond
                       [(or (empty? list-name) ...)
                        #t]
                       [else
                        (and
                          (let ([elem-name (first list-name)] ...)
                            computation)
                          (iterate (rest list-name) ...))]))])
           (iterate seq ...)))]))

(check-true
 (simple-for/and ([x (list 2 4 6)]) (even? x)))

(check-false
 (simple-for/and ([x (list 2 4 5)]) (even? x)))
