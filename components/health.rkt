#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (except-out (struct-out health) health)
         (rename-out (new-health health))
         ;get-health
         ;set-health
         ;change-health-by
         )

; This component is obsolete and here for compatibility
; the new stat system should be used instead

(component health (amount))

(define (update-health     g e c)
  (if (<= (health-amount c) 0)
      (die g e)
      e))

(new-component health?
               update-health)

#|(define (get-health e)
  (health-amount (get-component e health?)))

; === HANDLERS ===
(define (set-health amt)
  (lambda (g e)
    (update-entity e health? (new-health amt))))

(define (change-health-by amt)
  (lambda (g e)
    (update-entity e health? (new-health (+ (get-health e) amt)))))|#