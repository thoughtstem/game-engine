#lang racket

(require "../../game-engine.rkt"
         "../common/instructions.rkt"
         "../assets/ore-sprite.rkt"
         "../assets/spaceship-sprite.rkt"
         "../assets/space-bg-generator.rkt")

(define WIDTH  640)
(define HEIGHT 480)

(define bg-entity
  (sprite->entity (space-bg-sprite WIDTH HEIGHT 100)
                  #:name     "bg"
                  #:position (posn 0 0)))

(define spaceship-entity
  (sprite->entity spaceship-sprite
                  #:name       "ship"
                  #:position   (posn 100 100)
                  #:components (key-movement 5)))

(define (ore-entity p)
  (sprite->entity (ore-sprite (random 10))
                  #:position   p
                  #:name       "ore"
                  #:components (on-collide "ship" randomly-relocate-me)
                                ))

(define (randomly-relocate-me g e)
  (ore-entity (posn (random WIDTH)
                    (random HEIGHT))))

(start-game (instructions WIDTH HEIGHT "Use arrow keys to move")
            (ore-entity (posn 200 200))
            spaceship-entity
            bg-entity)
 
  