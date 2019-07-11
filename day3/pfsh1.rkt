#lang racket
(require "run.rkt"
         (for-syntax syntax/parse))
 
(provide #%module-begin
         (rename-out [pfsh:run run]))
 
(define-syntax (pfsh:run stx)
  (syntax-parse stx
    [(_ prog:id arg:id ...)
     #`(void (run (as-string prog) (as-string arg) ...))]))
 
(define-syntax (as-string stx)
  (syntax-parse stx
    [(_ sym:id)
     #`#,(symbol->string (syntax-e #'sym))]))