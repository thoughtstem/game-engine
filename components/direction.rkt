#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (except-out (struct-out direction) direction)
         (rename-out [new-direction direction])
         set-direction
         get-direction
         change-direction-by
         change-direction-by-random
         random-direction
         bounce-back)

(component direction (dir))

;(define (make-direction c)
;  (new-direction c))

(define (update-direction g e c) e)

(define (set-direction d)
 (lambda (g e)
     (update-entity e direction? (new-direction (modulo d 360)))))

(define (get-direction e)
  (direction-dir (get-component e direction?)))

(define (change-direction-by inc)
  (lambda (g e)
    (define d (get-direction e))
    (update-entity e direction? (new-direction (modulo (+ d inc) 360)))))

(define (change-direction-by-random min max)
  (lambda (g e)
    (define d (get-direction e))
    (update-entity e direction? (new-direction (modulo (+ d (random min (add1 max))) 360)))))

(define (random-direction min max)
  (lambda (g e)
     (update-entity e direction? (new-direction (modulo (random min (add1 max)) 360)))))

(define (bounce-back)
  (lambda (g e)
    (define dir (get-direction e))
    (define new-dir (+ dir 180))
    (update-entity e direction? (new-direction (modulo new-dir 360)))))

(new-component direction?
               update-direction) 