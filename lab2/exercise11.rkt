#lang racket

(require syntax/parse/define
         rackunit)
(require syntax/macro-testing)


(define-syntax (simple-for/fold stx)
  (syntax-parse stx
    [(_ ([acc:id base:expr])
        ([elem-name:id seq] ...) computation)
     #:declare seq (expr/c #'list?)
     (with-syntax ([(list-name ...) (generate-temporaries #'(elem-name ...))]
                   [how-many (length (syntax->list #'(elem-name ...)))])
       #'(letrec ([iterate
                   (λ (acc list-name ...)
                     (cond
                       [(or (empty? list-name) ...)
                        base]
                       [else
                        (let ([acc (iterate acc (rest list-name) ...)]
                              [elem-name (first list-name)] ...)
                          computation)]))])
           (iterate base seq.c ...)))]))


#;(check-equal?

   (simple-for/fold ([acc #t]) ([x (list 2 4 6)])
                    (and (even? x) acc)))

(check-exn #rx"simple-for/fold: contract violation\n  expected: list?" (thunk (simple-for/fold ([acc #t]) ([x 666])
                                                                                               (and (even? x) acc))))

