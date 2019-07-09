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

(define-syntax (simple-for/fold stx)
  (syntax-parse stx
    [(_ ([acc base])
        ([elem-name seq] ...) computation)
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
           (iterate base seq ...)))]))


(check-equal?
 (simple-for/and  ([x (list 2 4 6)])
                  (even? x))
 (simple-for/fold ([acc #t]) ([x (list 2 4 6)])
                  (and (even? x) acc)))

