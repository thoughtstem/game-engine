#lang racket

(provide pretty-print-game
         pretty-print-entity
         pretty-print-component)

(require "./base.rkt")

(define (pretty-print-game g)
  (displayln (~a "GAME:" ))
   (for ([e (game-entities g)])
     (pretty-print-entity e)))

(define (pretty-print-entity e)
  (displayln (~a "  Entity: " (entity-id e)))
  (for ([c (entity-components e)])
    (pretty-print-component c)) )

(define (pretty-print-component c)
  (displayln (~a "    COMPONENT: " (vector-ref c 1)))
  
  (define vs (vector->list c))

  (for ([v vs])
    (displayln (~a "      " v))))

