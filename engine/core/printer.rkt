#lang racket

(provide pretty-print-game
         pretty-print-games
         pretty-print-entity
         pretty-print-entity-string
         pretty-print-component)

(require "./base.rkt")

(define (pretty-print-games . gs)
  (for ([g gs])
    (pretty-print-game g)))

(define (pretty-print-game g)
  (displayln (~a "GAME:" ))
   (for ([e (game-entities g)])
     (pretty-print-entity e)))

(define (pretty-print-entity e)
  (displayln (~a "  Entity: " (entity-id e) " " (if (entity-changed? e) "[CHANGED!]" "")))
  (for ([c (entity-components e)])
    (pretty-print-component c)) )

(define (pretty-print-component c)
  (displayln (~a "    COMPONENT: " (vector-ref c 1) ", " (vector-ref c 2)))
  
  (define vs (vector->list c))

  (for ([v (drop vs 4)])
    (displayln (~a "      " v)))
  
  (displayln (~a "      " (vector-ref c 3))))


(define (pretty-print-entity-string e)
  (with-output-to-string
    (thunk (pretty-print-entity e))))

