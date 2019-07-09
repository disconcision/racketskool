#lang racket

(require syntax/parse/define
         rackunit)

(define-syntax (define-dfa-like stx)
  (syntax-parse stx
    [(_ dfa-name
        #:start start-state
        #:accumulator acc-name acc-init
        (#:state state-name
         #:value state-val
         [p? (new-state new-acc)] ...)
        ...)
     #'(define (dfa-name init-inputs)
         (let loop
           ([state 'start-state]
            [acc-name acc-init]
            [inputs init-inputs])
           (case state
             ['state-name
              (match inputs
                [`(,(? p?) . ,ps)
                 (loop 'new-state new-acc ps)]
                ...
                [`() state-val])]
             ...)))]))

(define (any? x) #t)
(define-dfa-like length-of-odd
  #:start even
  #:accumulator len 0
  (#:state even
   #:value #f
   [any? (odd (add1 len))])
  (#:state odd
   #:value len
   [any? (even (add1 len))]))

(check-false (length-of-odd '())) ; => #f
(check-equal? 1 (length-of-odd '(0))) ; => 1
(check-false (length-of-odd '(0 1))) ; => #f
(check-equal? 3 (length-of-odd '(1 0 0))) ; => 3
