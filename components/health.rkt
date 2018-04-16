#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (struct-out health))

(struct health (amount))

(define (update-health     g e c)
  (if (= 0 (health-amount c))
      (die g e)
      e))

(new-component health?
               update-health) 