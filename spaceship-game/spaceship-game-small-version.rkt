#lang racket

(require "../game-engine.rkt"
         "./assets/spaceship-sprite.rkt"
         "./assets/ore-sprite.rkt"
         "./assets/space-bg-generator.rkt")

(define WIDTH  640)
(define HEIGHT 480)

(define bg-entity
  (sprite->entity (space-bg-sprite WIDTH HEIGHT 100)
                  #:position (posn 0 0)
                  #:name     "bg"))

(define (spaceship-entity p)
  (sprite->entity spaceship-sprite
                  #:position   p
                  #:name       "ship"
                  #:components (key-movement 5)
                               (physical-collider)
                               ;(key-animator 'none spaceship-animator)
                               ))

(define (enemy-entity p)
  (sprite->entity (spaceship-animator 'left)
                  #:position   p
                  #:name       "enemy"
                  #:components  (physical-collider)
                  ))

(define (ore-entity p)
  (sprite->entity (ore-sprite (random 10))
                  #:position   p
                  #:name       "ore"
                  #:components ;(key-movement 1)
                               (on-collide "ship" randomly-relocate-me)
                                ))

(define (randomly-relocate-me g e)
  (ore-entity (posn (random WIDTH)
                    (random HEIGHT))))

(start-game (spaceship-entity (posn 100 400))
            (ore-entity       (posn 200 400))
            (enemy-entity     (posn 300 300))
            bg-entity)

  