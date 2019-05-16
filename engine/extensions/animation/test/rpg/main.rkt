#lang racket

(require "../../main.rkt"
         "./images.rkt"
         "./input.rkt"
         "./animations.rkt")

(define hero
  (entity
    (position (posn 200 200))
    (hero-movement)
    (hero-animation)))

(define world
  (entity
    (position (posn 0 0))

    ;TODO: Figure out traversal
    (sprite bg-sprite)))

(define rpg
  (game 
    input-manager 
    world
    bg))     

(play! rpg)

