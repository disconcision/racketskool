#lang s-exp "pfsh5.rkt"


#;(run ls "-1")

(&& (run test -f demo.txt)
    (run cat demo.txt))