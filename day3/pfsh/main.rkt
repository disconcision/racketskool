#lang racket
(require "run.rkt"
         racket/port
         (for-syntax syntax/parse
                     racket))
 
(provide 
 < >
 #;#%module-begin
 (rename-out [pfsh:#%module-begin #%module-begin]
             [pfsh:run #%app]
             [pfsh:top #%top]
             #;[pfsh:< <]
             #;[pfsh:> >]
             [pfsh:define define]
             [pfsh:datum #%datum]
             [pfsh:top-interaction #%top-interaction]
             [pfsh:string-append string-append]))
 
(module reader syntax/module-reader
  pfsh)



(define-syntax (pfsh:#%module-begin stx)
  (define top-level-list (syntax->list stx))
  (define (key x)
    (if (pair? (syntax-e x))
        'paren
        (syntax-line x)))
  (define (same? x y)
    (if (or (equal? x 'paren)
            (equal? y 'paren))
        #f
        (equal? x y)))
  (define line-groups
    (group-by key (rest top-level-list) same?))
  (define final
    (for/list ([s line-groups])
      (if (pair? (syntax-e (first s)))
          (first s)
          (datum->syntax stx s))))
 #`(#%module-begin #,@final)
  
  )

(define-syntax (< stx)
  (raise-syntax-error '< "bare <" #'stx))

(define-syntax (> stx)
  (raise-syntax-error '> "bare >" #'stx))

(define-syntax (pfsh:run stx)
  (syntax-parse stx
    #:literals (< >)
    [(_ prog arg ... > no!! < bad!!)
     (raise-syntax-error 'pfsh:run ">_< wrong order!!!"
                         #'prog)]
    [(_ prog arg ... < in-stream > out-stream)
     #'(pfsh:run (pfsh:run prog arg ... < in-stream) > out-stream)]
    [(_ expr ... > new-name:id)
     #'(define new-name (with-output-to-string
                          (lambda ()
                            expr ...)))]
    [(_ prog arg ... < stream:expr)
     #'(with-input-from-string
           stream
         (lambda ()
           (pfsh:run prog arg ...)))]
    [(_ prog arg ...)
     #`(void (run prog arg ...))]))
 
(define-syntax (pfsh:top stx)
  (syntax-parse stx
    [(_ . sym:id)
     #`#,(symbol->string (syntax-e #'sym))]))
 
(define-syntax (pfsh:define stx)
  (syntax-parse stx
    [(_ stream:id expr)
     #'(define stream (with-output-to-string
                        (lambda ()
                          expr)))]
    [(_ (proc:id arg:id ...) expr)
     #'(begin
         (define (actual-proc arg ...)
           expr)
         (define-syntax (proc stx)
           (syntax-parse stx
             [(_ arg ...) #'(actual-proc arg ...)])))]))
 
(pfsh:define (pfsh:string-append arg1 arg2)
             (string-append arg1 arg2))
 
(define-syntax (pfsh:datum stx)
  (syntax-parse stx
    [(_ . s:string) #'(#%datum . s)]
    [(_ . other)
     (raise-syntax-error 'pfsh
                         "only literal strings are allowed"
                         #'other)]))
 
(define-syntax (pfsh:top-interaction stx)
  (syntax-parse stx
    [(_ . form) #'form]))