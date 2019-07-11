#lang s-exp "pfsh3.rkt"

(define l (run ls))
l
(run wc -l < l)