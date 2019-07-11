#lang s-exp "pfsh6.rkt"

(run test -f demdo.txt)
#;(run ls "-1")

(&& (run test -f demo.txt)
    (run cat demo.txt))