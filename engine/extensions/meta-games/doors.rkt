#lang racket

(provide door door-manager near-avatar?)

(require "../../core/main.rkt"
         "../rendering/animated-sprite.rkt"
         "../common-components/main.rkt")

(define (near-avatar? n)
  (> n 
     (distance (get-position)
               (get 'avatar 'position))))

(define (door #:detect (condition? (thunk* (near-avatar? 25)))
              #:to to . cs)
  (entity
    (name 'door)
    cs
    (sub-game #f
              (if (condition?) (to) #f))))

(define-component door-destination game?)

(define (doors-in g)
  (filter 
    (has-name 'door)
    (game-entities g)))

(define (active-door? e)
  (get-sub-game e))


(define (door-manager start)
  (entity
    (sub-game start
              (if (get-door-destination)
                (get-door-destination)  
                (tick (get-sub-game))))

    (door-destination #f
                      (let ([active-door (findf
                                           active-door? 
                                           (doors-in (get-sub-game)))])

                         (if active-door
                           (get-sub-game active-door)
                           #f)))
    
    (also-render start (get-sub-game))))


