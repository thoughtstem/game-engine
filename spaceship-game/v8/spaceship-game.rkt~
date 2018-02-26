#lang racket

(require "../../game-engine.rkt"
         "../common/instructions.rkt"
         "../common/game-over-screen.rkt"
         "../assets/ore-sprite.rkt"
         "../assets/spaceship-sprite.rkt"
         "../assets/space-bg-generator.rkt")

(define WIDTH  640)
(define HEIGHT 480)

(define bg-entity
  (sprite->entity (space-bg-sprite WIDTH HEIGHT 100)
                  #:name     "bg"
                  #:position (posn 0 0)))

(define (spaceship-entity)
  (sprite->entity spaceship-sprite
                  #:name       "ship"
                  #:position   (posn 100 100)
                  #:components (key-movement 5)
                               (on-collide "ore" (change-speed-by 1))
                               (on-collide "enemy" die))) 

(define (ore-entity p)
  (sprite->entity (ore-sprite (random 10))
                  #:position   p
                  #:name       "ore"
                  #:components (on-collide "ship" (randomly-relocate-me 0 WIDTH 0 HEIGHT))))

(define (enemy-entity p)
  (sprite->entity (spaceship-animator 'left)
                  #:position   p
                  #:name       "enemy"
                  #:components  (every-tick (move-up-and-down #:min   0  
                                                              #:max   HEIGHT
                                                              #:speed 10))))

(define (lost? g e)
  (not (get-entity "ship" g)))

(define (won? g e) 
  (define speed (get-speed (get-entity "ship" g)))
  (>= speed 10))

(start-game (instructions WIDTH HEIGHT "Use arrow keys to move")
            (game-over-screen won? lost?)
            (ore-entity (posn 400 400))
            (spaceship-entity)
            (enemy-entity (posn 300 300))
            bg-entity)


