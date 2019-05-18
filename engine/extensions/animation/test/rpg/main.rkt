#lang racket

(require "../../main.rkt"
         "./images.rkt"
         "./input.rkt"
         "./animations.rkt"
         2htdp/image)

(define (hero)
  (entity
    (name 'avatar) 
    (position (posn 200 200))

    ;TODO: Figure out how to abstract these magic numbers
    ;Presumably this sets up the bb and location, but it should be kinematic or something?  The position in game should propagage to chipmunk land.
    (physics-system 200 200 20 20)

    (hero-movement)
    (hero-animation)))

(define (world)
  (entity
    (position (posn 0 0))
    (sprite bg-sprite)))

(define edge (register-sprite (rectangle 400 10 'solid 'red)))

(define (rpg)
  (displayln "RPG start")
  (game 
    input-manager 
    ;Seems to be moving slower as framerate slows.  Make movement fps independent.
    (hero)

    (door #:to rpg
      ;(Note, it does seem like physics-system should get passed in.  A "door" might want to move around or be non-physcal or something.  The less we assume in these entities, the more flexibility.)

      ;Presumably this sets up the bb and location.  I think they should be non-kinematic and static somehow.  And the position in chipmunk land should propagate to in-game.
      (physics-system 200 0 400 10)
      (position (posn 200 0))
      (sprite edge)
      (rotation 0))

    (door #:to rpg
      (physics-system 400 200 400 10)
      (position (posn 400 200))
      (sprite edge)
      (rotation (/ pi 2)))

    (door #:to rpg
      (physics-system 200 400 400 10)
      (position (posn 200 400))
      (sprite edge)
      (rotation 0))

    (door #:to rpg
      (physics-system 0 200 400 10)
      (position (posn 0 200))
      (sprite edge)
      (rotation (/ pi 2)))
    (world)))

(play! 
  (game
    physics-manager
    (door-manager (rpg))))



