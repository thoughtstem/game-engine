#lang racket

(require "../game-entities.rkt"
         "../components/every-tick.rkt"
         "../components/after-time.rkt"
         "../components/speed.rkt"
         "../components/direction.rkt"
         "../components/on-start.rkt"
         "../components/backdrop.rkt"
         "../component-util.rkt"
         "../ai.rkt"
         "./sprite-util.rkt"
         2htdp/image
         posn)

(provide custom-particles)

(define green-star (star 5 'solid 'green))

(define (custom-particles
         #:sprite (sprite green-star)
         #:speed  (s 10)
         #:scale-each-tick (scale-each-tick 1.01)
         #:direction-min-max (dir '(0 360))
         #:particle-time-to-live (ttl 100)
         #:system-time-to-live (sttl 10))

  (precompile! green-star)
  
  (define particle 
    (sprite->entity sprite
                    #:position (posn 0 0)
                    #:name "particle"
                    #:components
                    (speed s)
                    (direction 0)
                    (every-tick (do-many
                                 (move)
                                 (scale-sprite scale-each-tick)))
                    (on-start (random-direction (first dir)
                                                (second dir)))
                    (after-time ttl die)))

  (sprite->entity empty-image
                  #:position (posn 0 0)
                  #:name "particle-system"
                  #:components
                  (every-tick (spawn-on-current-tile particle))
                  (after-time sttl die)) )
