#lang racket

(require "../../main.rkt"
         "./images.rkt"
         "./input.rkt"
         "./animations.rkt"
         2htdp/image
         )

(define hero
  (entity
    (name 'avatar) ;Not having this is a gotcha with door-manager.  Generalize.  At least pass in somehow.
    (position (posn 200 200))
    (hero-movement)
    (hero-animation)))

(define world
  (entity
    (position (posn 0 0))

    ;TODO: Figure out traversal
    (sprite bg-sprite)))

(define edge (register-sprite (rectangle 400 10 'solid 'red)))

(define (rpg)
  (displayln "RPG start")
  (game 
    input-manager 
    ;Seems to be moving slower as framerate slows.  Make movement fps independent.
    hero

    ;It's "detecting collision" in the top left corner, because it's using near-avatar, which is 25 units from 0,0
    ; 
    ;But really it should use the physics system.  Should we sort of roll our own first, for testing?
    (door #:to rpg
      (position (posn 0 0))
      (sprite edge)
      ;Rotation looks wrong, try fixing with hotswap?  And position seems off too.
      (rotation 0))

    (door #:to rpg
      (position (posn 400 0))
       ;Need to be able to rotate.  How to pass components into entity constructors like this? Maybe all should assume a #:rest components and use keywords for any special configs?  Then it matches entity really well.
      (sprite edge)
      (rotation (/ pi 4)))

    (door #:to rpg
      (position (posn 400 400))
      (sprite edge)
      (rotation 0))

    (door #:to rpg
      (position (posn 0 400))
      (sprite edge)
      (rotation (/ pi 4)))
    world
))

(play! 
  (game
    (door-manager (rpg))))



