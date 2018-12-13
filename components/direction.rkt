#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (struct-out direction)
         set-direction
         get-direction
         change-direction-by
         change-direction-by-random
         random-direction
         bounce-back)

(struct direction (dir))

(define (update-direction g e c) e)

(define (set-direction d)
 (lambda (g e)
     (update-entity e direction? (direction (modulo d 360)))))

(define (get-direction e)
  (direction-dir (get-component e direction?)))

(define (change-direction-by inc)
  (lambda (g e)
    (define d (get-direction e))
    (update-entity e direction? (direction (modulo (+ d inc) 360)))))

(define (change-direction-by-random min max)
  (lambda (g e)
    (define d (get-direction e))
    (update-entity e direction? (direction (+ d (random min (add1 max)))))))

(define (random-direction min max)
  (lambda (g e)
     (update-entity e direction? (direction (modulo (random min (add1 max)) 360)))))

(define (bounce-back)
  (lambda (g e)
    (define dir (get-direction e))
    (define new-dir (+ dir 180))
    (update-entity e direction? (direction (modulo new-dir 360)))))

(new-component direction?
               update-direction) 