#lang racket 

(provide direction facing
         get-direction get-facing)
(require "../../core/main.rkt")

(define-component direction posn?)
(define-component facing symbol?)                                         
