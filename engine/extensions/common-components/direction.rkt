#lang racket 

(provide direction facing
         get-direction get-facing
         distance)
(require "../../core/main.rkt")

(define-component direction posn?)
(define-component facing symbol?)                                         
;Abstract somewhere better?
(define (distance p1 p2)
  (define x-diff (- (posn-x p1)
                    (posn-x p2)))
  (define y-diff (- (posn-y p1)
                    (posn-y p2)))
  (sqrt
    (+
      (sqr x-diff) 
      (sqr y-diff))))
