#lang racket


(require syntax/parse/define
         (for-syntax syntax/parse
                     racket/stxparam)
         racket/stxparam
         syntax/macro-testing
         rackunit)


(define-syntax-parameter break
  (λ (stx) (raise-syntax-error 'break "no!!!!!")))

(define-syntax-parameter continue
  (λ (stx) (raise-syntax-error 'break "no!!!!!")))

(define-syntax (while stx)
  (syntax-parse stx
    [(_ cond:expr
        body:expr ...)
     #'(let/ec k-break
         (let loop []
           (when cond
             (syntax-parameterize
                 ([break (make-rename-transformer #'k-break)])
               (let/ec k-continue
                 (syntax-parameterize
                     ([continue (make-rename-transformer #'k-continue)])
                   body ...))
               (loop)))))]))

(define x 10)

(while (> x 0)
       (set! x (sub1 x))
       (when (odd? x) (continue x))
       (println x))