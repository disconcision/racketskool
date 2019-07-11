#lang s-exp "pfsh3.rkt"

(define l (run ls))
(run wc -l < l)