#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (struct-out speed)
         set-speed
         get-ai-speed
         change-ai-speed-by
         random-speed)

(struct speed (spd))

(define (update-speed g e c) e)

(define (set-speed d)
 (lambda (g e)
     (update-entity e speed? (speed d))))

(define (get-ai-speed e)
  (speed-spd (get-component e speed?)))

(define (change-ai-speed-by inc)
  (lambda (g e)
    (define s (get-ai-speed e))
    (update-entity e speed? (speed (+ s inc)))))

(define (random-speed min max)
  (lambda (g e)
    (define new-min (exact-round (* min 100)))
    (define new-max (exact-round (* max 100)))
    (update-entity e speed? (speed (/ (random new-min (add1 new-max)) 100)))))

(new-component speed?
               update-speed) 