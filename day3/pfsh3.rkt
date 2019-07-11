#lang racket
(require "run.rkt"
         (for-syntax syntax/parse
                     racket/port ))
 
(provide #%module-begin
         (rename-out [pfsh:run run]
                     [pfsh:define define]))
 
(define-syntax (pfsh:run stx)
  (syntax-parse stx
    [(_ prog:id arg:id ... (~datum <) redir-string)
     #`(void (with-input-from-string
               redir-string
               (thunk (run (as-string prog) (as-string arg) ...))))]
    [(_ prog:id arg:id ...)
     #`(void (run (as-string prog) (as-string arg) ...))]))
 
(define-syntax (as-string stx)
  (syntax-parse stx
    [(_ sym:id)
     #`#,(symbol->string (syntax-e #'sym))]))

(define-syntax (pfsh:define stx)
  (syntax-parse stx
    [(_ name:id (~and cmd ((~datum run) args ...)))
     #`(define name
         (with-output-to-string (thunk cmd)))]
    [(_ name:id whatever)
     (raise-syntax-error 'define "use run!!!" #'stx)
     #; #`(define name whatever)
     ]))