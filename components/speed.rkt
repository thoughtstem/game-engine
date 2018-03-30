#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (struct-out speed)
         set-speed
         get-ai-speed
         random-speed)

(struct speed (spd))

(define (update-speed g e c) e)

(define (set-speed d)
 (lambda (g e)
     (update-entity e speed? (speed d))))

(define (get-ai-speed e)
  (speed-spd (get-component e speed?)))

(define (random-speed min max)
  (lambda (g e)
     (update-entity e speed? (speed (random min (add1 max))))))

(new-component speed?
               update-speed) 