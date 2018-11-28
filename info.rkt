#lang info

;(define collection 'multi)

(define version "0.0.1")
(define scribblings '(("game-engine.scrbl" ())))

(define deps '("threading"
               "drracket"
               "htdp-lib"
               "https://github.com/jeapostrophe/lux.git#f6edd2e"
               "jack-posn"
               ;"rsound"
               "https://github.com/thoughtstem/racket-chipmunk.git"
               "base"))

(define compile-omit-paths '(
  "test"
))
