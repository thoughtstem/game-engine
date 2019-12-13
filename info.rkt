#lang info

;(define collection 'multi)

(define version "0.0.1")
(define scribblings '(("scribblings/game-engine.scrbl" ())))

(define deps '("threading"
               "memoize"
               "https://github.com/jeapostrophe/mode-lambda.git#0858b6d"
               "drracket"
               "htdp-lib"
               "https://github.com/jeapostrophe/lux.git" ;was frozen at #f6edd2e  
               "jack-posn"
          ;     "rsound"
               '("https://github.com/thoughtstem/racket-chipmunk.git#master" #:version "0.0.2")
               "base"))

(define compile-omit-paths '(
  "test"
))
