#lang racket

(provide start-game)

(provide (all-from-out "./engine/core.rkt"))

(require "./engine/core.rkt")

(define (start-game . initial-world)
  (define larger-state (game (flatten initial-world)
                             '()
                             button-states
                             button-states
                             '()))

  (define g
    (physics-start (uniqify-ids larger-state)))

  
  (final-state
   (lux-start g)))












