#lang racket

(require syntax/parse/define
         rackunit)

(define-syntax (monad-do stx)
  (syntax-parse stx
    [(_ return bind [x #:<- mx] more ...)
     #'(bind mx
             (λ (x) (monad-do return bind more ...)))]
    [(_ return bind [#:ret expr])
     #'(return expr)]))



(define ((sum-both return bind) mx my)
  (monad-do return bind
            [x #:<- mx]
            [y #:<- my]
            [#:ret (+ x y)]))

(check-equal?
 (map (sum-both (λ (x) x)
               (λ (ma f) (and ma (f ma))))
     (list 3 #f 5)
     (list #f 4 6))
 (list #f #f 11))

(check-equal?
 ((sum-both (λ (x) (list x))
           (λ (ma f) (append-map f ma)))
 (list 1 2 3)
 (list 3 4 5))
 (list 4 5 6 5 6 7 6 7 8))