#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (struct-out direction)
         set-direction
         get-direction
         random-direction
         bounce-back)

(struct direction (dir))

(define (update-direction g e c) e)

(define (set-direction d)
 (lambda (g e)
     (update-entity e direction? (direction d))))

(define (get-direction e)
  (direction-dir (get-component e direction?)))

(define (random-direction min max)
  (lambda (g e)
     (update-entity e direction? (direction (random min (add1 max))))))

(define (bounce-back)
  (lambda (g e)
    (define dir (get-direction e))
    (define new-dir (+ dir 180))
    (update-entity e direction? (direction new-dir))))

(new-component direction?
               update-direction) 