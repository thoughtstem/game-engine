#lang racket

(require "../../main.rkt"
         "./images.rkt"
         "./input.rkt"
         "./animations.rkt"
         2htdp/image)

(define (hero)
  (entity
    (name 'avatar) 

    (physics-system 200 200 20 20
                    #:mass 1
                    #:update (thunk* (posn-scale 100 (as-posn (get-current-input)))))

    ;TODO: Would this work above physics-system?  Not sure...  What's the normal form?
    (position (posn 200 200)
              (get-physics-position))

    ;TODO: hero-movement hides a position component, which resulted in me putting two position components on.  For one: that should have thrown an error.  For another, maybe these systems shouldn't return lists, but rather just entities and a function to get the subcomponents (e.g. sub-position) from there and propagate it up
    #;
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
    (physics-manager)
    ;Seems to be moving slower as framerate slows.  Make movement fps independent.
    (hero)

    (door #:to rpg
      ;(Note, it does seem like physics-system should get passed in.  A "door" might want to move around or be non-physcal or something.  The less we assume in these entities, the more flexibility.)

      ;Presumably this sets up the bb and location.  I think they should be non-kinematic and static somehow.  And the position in chipmunk land should propagate to in-game.
      (physics-system 200 100 400 10
                      #:mass 1000000
                      )
      (position (posn 200 100))
      (sprite edge)
      (rotation 0))

    #;
    (door #:to rpg
      (physics-system 400 200 400 10)
      (position (posn 400 200))
      (sprite edge)
      (rotation (/ pi 2)))

    #;
    (door #:to rpg
      (physics-system 200 400 400 10)
      (position (posn 200 400))
      (sprite edge)
      (rotation 0))

    #;
    (door #:to rpg
      (physics-system 0 200 400 10)
      (position (posn 0 200))
      (sprite edge)
      (rotation (/ pi 2)))

    (world)))


#;
(debug
  (play!
    (game
      (door-manager (rpg)))))

(play! 
  (game
    (door-manager (rpg))))



