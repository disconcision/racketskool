#lang racket

(require syntax/parse/define
         rackunit)

(define-syntax (define-dfa stx)
  (syntax-parse stx
    [(_ dfa-name #:start start-state
        (#:state state-name
         state-type:keyword
         [p? new-state] ...)
        ...)
     #'(define (dfa-name init-inputs)
         (let loop
           ([state 'start-state]
            [inputs init-inputs])
           (case state
             ['state-name
              (match inputs
                [`(,(? p?) . ,ps)
                 (loop 'new-state ps)]
                ...
                [`()  (equal? 'state-type '#:accepting)])]
             ...)))]))

(define (any? x) #t)
(define-dfa even-length
  #:start even
  (#:state even
   #:accepting
   [any? odd])
  (#:state odd
   #:rejecting
   [any? even]))

(check-true  (even-length '())) ; => #t
(check-false (even-length '(0))) ; => #f
(check-true  (even-length '(0 0))) ; => #t
(check-false (even-length '(1 1 1))) ; => #f

(define ((is? x) y) (equal? x y))
(define-dfa represents-odd
  #:start not-odd
  (#:state not-odd
   #:rejecting
   [(is? 0) not-odd]
   [(is? 1) odd])
  (#:state odd
   #:accepting
   [(is? 0) not-odd]
   [(is? 1) odd]))

(check-false (represents-odd '())) ; => #f
(check-false (represents-odd '(0))) ; => #f
(check-true  (represents-odd '(0 1))) ; => #t
(check-true  (represents-odd '(1 0 1))) ; => #t
