#lang racket

(require "../game-entities.rkt"
         "../components/every-tick.rkt"
         "../components/after-time.rkt"
         "../components/animated-sprite.rkt"
         "../components/speed.rkt"
         "../components/direction.rkt"
         "../components/on-start.rkt"
         "../components/on-edge.rkt"
         "../components/backdrop.rkt"
         "../component-util.rkt"
         "../ai.rkt"
         "./sprite-util.rkt"
         2htdp/image
         posn)

(provide custom-particles)

(define green-star (star 5 'solid 'black))

(define (custom-particles
         #:sprite (sprite green-star)
         #:speed  (s 5)
         #:scale-each-tick (scale-each-tick 1.01)
         #:direction-min-max (dir '(0 360))
         #:particle-time-to-live (ttl 25)
         #:system-time-to-live (sttl 10))

  (precompile! sprite)
  (define (randomize-color)
    (lambda (g e)
      (define as (get-component e animated-sprite?))
      (define new-c (first (shuffle (list 'red 'orange 'yellow 'green 'blue 'indigo 'violet))))
      (update-entity e animated-sprite? (struct-copy animated-sprite as
                                                     [color new-c]))))
  
  (define particle 
    (sprite->entity sprite
                    #:position (posn 0 0)
                    #:name "particle"
                    #:components
                    (speed s)
                    (direction 0)
                    (every-tick (do-many
                                 (randomize-color)
                                 (scale-sprite scale-each-tick)
                                 (change-direction-by-random -15 15)
                                 (move)
                                 ))
                    (on-start (do-many (randomize-color)
                                       (random-direction (first dir)
                                                         (second dir))))
                    (after-time ttl die)
                    (on-edge 'left die)
                    (on-edge 'right die)
                    (on-edge 'top die)
                    (on-edge 'bottom die)))

  (sprite->entity empty-image
                  #:position (posn 0 0)
                  #:name "particle-system"
                  #:components
                  (every-tick (spawn-on-current-tile particle))
                  (after-time sttl die)) )
