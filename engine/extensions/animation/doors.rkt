#lang racket

(provide door door-manager near-avatar?)

(require "../../core/main.rkt"
         ;TODO: All of animated sprite for position???
         "./animated-sprite.rkt"
         "./common-components.rkt")

;Abtract into doors.rkt
(define (near-avatar? n)
  (> n 
     (distance (get-position)
               (get 'avatar 'position))))


(define (door p s to)
  (entity
    (name 'door)
    (position p)
    s ;sprite
    (sub-game #f
              (if (near-avatar? 25)
                (to)  
                #f))))

(define-component door-destination game?)

(define (doors-in g)
  (filter 
    (has-name 'door)
    (game-entities g)))

(define (active-door? e)
  (get-sub-game e))


(define (door-manager start)
  ;If one of the doors in the sub-game has a sub-game,
  ;  replace this sub-game with that one...
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
