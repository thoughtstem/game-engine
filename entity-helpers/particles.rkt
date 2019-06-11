#lang racket

(require "../game-entities.rkt"
         "../components/every-tick.rkt"
         "../components/after-time.rkt"
         "../components/do-every.rkt"
         "../components/animated-sprite.rkt"
         "../components/speed.rkt"
         "../components/direction.rkt"
         "../components/on-start.rkt"
         "../components/on-edge.rkt"
         "../components/backdrop.rkt"
         "../components/storage.rkt"
         "../component-util.rkt"
         "../ai.rkt"
         "./sprite-util.rkt"
         2htdp/image
         posn
         threading)

(provide custom-particles
         particle-system)

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
                  ;(every-tick (spawn-on-current-tile particle))
                  (on-start (do-many (spawn-on-current-tile particle)
                                     (spawn-on-current-tile particle)
                                     (spawn-on-current-tile particle)
                                     (spawn-on-current-tile particle)
                                     (spawn-on-current-tile particle)))
                  (do-every 5 (do-many (spawn-on-current-tile particle)
                                       (spawn-on-current-tile particle)
                                       (spawn-on-current-tile particle)
                                       (spawn-on-current-tile particle)
                                       (spawn-on-current-tile particle)))
                  (after-time sttl die)))

(define (particle-system #:sprite (sprite green-star)
                         #:speed  (s 5)
                         #:scale-each-tick (scale-each-tick 1.01)
                         #:direction-min-max (dir '(0 360))
                         #:particle-time-to-live (ttl 25)
                         #:system-time-to-live (sttl 10))
  (precompile! sprite)
  
  (define (particle-sprite)
    (~> (ensure-sprite sprite)
        (set-x-offset (random -5 6) _)
        (set-y-offset (random -5 6) _)
        (set-sprite-color (first (shuffle (list 'red 'orange 'yellow 'green 'blue 'indigo 'violet))) _)))

  (define particle-sprites
    (list (particle-sprite)
          (particle-sprite)
          (particle-sprite)
          (particle-sprite)
          (particle-sprite)))
  
  ;(define random-x (random -5 6))
  ;(define random-y (random -5 6))
  
  (define particle-id (random 10000))

  (define (do-particle-fx g e)
    (define particle-sprites (first (get-storage-data (~a "particle-" particle-id) e)))
    (define current-particle-sprites (get-components e (apply or/c (map (curry component-eq?) particle-sprites))))
    ;change x, y, and scale
    (define new-particle-sprites (map (λ (s)
                                        (~> s
                                            (change-x-offset (random -5 6) _)
                                            (change-y-offset (random -5 6) _)
                                            (scale-xy scale-each-tick _)))
                                      current-particle-sprites))
    (~> e
        (remove-components _ (apply or/c (map (curry component-eq?) current-particle-sprites)))
        (add-components _ new-particle-sprites)))

  (define particle-fx-component (every-tick do-particle-fx))
  
  (define (remove-particle-system g e)
    (define particle-components (get-storage-data (~a "particle-" particle-id) e))
    (~> e
        (remove-components _ (apply or/c (map (curry component-eq?) (first particle-components))))
        (remove-components _ (curry component-eq? (second particle-components)))
        (remove-storage (~a "particle-" particle-id) _)))
    
  (flatten (list particle-sprites
                 (storage (~a "particle-" particle-id) (list particle-sprites particle-fx-component))
                 particle-fx-component
                 (after-time ttl remove-particle-system))))
