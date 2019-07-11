#lang racket
(require "run.rkt"
         syntax/parse
         syntax/wrap-modbeg
         (for-syntax syntax/parse
                     racket
                     
                     racket/port ))
 
(provide 
         &&
         #;#%module-begin
         (rename-out [pfsh:run run]
                     [pfsh:#%module-begin #%module-begin]
                     [pfsh:define define]))

(begin-for-syntax
  (define-syntax-class valid-arg
  (pattern (~or a:id a:number a:string))))

(define-syntax (pfsh:#%module-begin stx)
  (syntax-parse stx
   #; [(_ (~and def ((~datum define) _ ...)) more ...)
     #'(#%module-begin  (&& xs ...))]
    [(_ xs ...)
     #'(#%module-begin  (&& xs ...))])
  #;(make-wrapping-module-begin
   #'void))

(define-syntax (pfsh:run stx)
  (syntax-parse stx
    [(_ prog:id arg:valid-arg ... (~datum <) redir-string)
     #`(identity (with-input-from-string
               redir-string
               (thunk (run (as-string prog) (as-string arg) ...))))]
    [(_ prog:id arg:valid-arg ...)
     #`(identity (run (as-string prog) (as-string arg) ...))]))
 
(define-syntax (as-string stx)
  (syntax-parse stx
    [(_ sym:valid-arg)
     #`#,(~a (syntax-e #'sym))]))

(define-syntax (pfsh:define stx)
  (syntax-parse stx
    [(_ name:id (~and cmd ((~datum run) args ...)))
     #`(define name
         (with-output-to-string (thunk cmd)))]
    [(_ name:id whatever)
     (raise-syntax-error 'define "use run!!!" #'stx)
     #; #`(define name whatever)
     ]))

(define-syntax (&& stx)
  (syntax-parse stx
    [(_ rs ...)
     #'(and rs ...)]))