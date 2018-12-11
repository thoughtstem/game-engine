#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (struct-out health)
         get-health
         set-health
         change-health-by
         )

(struct health (amount))

(define (update-health     g e c)
  (if (<= (health-amount c) 0)
      (die g e)
      e))

(new-component health?
               update-health)

(define (get-health e)
  (health-amount (get-component e health?)))

; === HANDLERS ===
(define (set-health amt)
  (lambda (g e)
    (update-entity e health? (health amt))))

(define (change-health-by amt)
  (lambda (g e)
    (update-entity e health? (health (+ (get-health e) amt)))))