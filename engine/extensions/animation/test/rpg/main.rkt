#lang racket

(require "../../main.rkt"
         "./images.rkt"
         "./animations.rkt"
         2htdp/image)

(define-component speed number?)

(define (hero)
  (entity
    (name 'avatar) 

    (position (posn 200 200)
              ;TODO: Mushy collisions.  Maybe don't set velocity every tick.  See chipmunk docs.
              (get-physics-position))

    (rotation 0
              (get-physics-rotation))

    (physics-system 20 20
                    #:mass 1
                    #:update 
                    (thunk* (posn-scale (* 25
                                           (get-delta-time)) 
                                        (as-posn (get-current-input)))))

    (hero-animation)))

(define (world)
  (entity
    (position (posn 0 0))
    (sprite bg-sprite)))

(define edge (register-sprite (rectangle 400 10 'solid 'red)))

(define (rpg)
  (displayln "RPG start")
  (game 
    input-manager ;TODO: Managers should be functions?

    (physics-manager)

    (time-manager)

    ;TODO: Jittering with framerate, multiply by delta time??
    ;Make a time-manager ....


    (hero)

    (door #:to rpg
      (physics-system 400 10
                      #:mass 1000000 ;TODO: Static?
                      )
      (position (posn 200 0))
      (sprite edge)
      (rotation 0))

    (door #:to rpg
      (physics-system 400 10
                      #:mass 1000000 ;Static?
                      )
      (position (posn 400 200))
      (sprite edge)
      (rotation (/ pi 2)))

    (door #:to rpg
      (physics-system 400 10
                      #:mass 1000000 ;Static?
                      )
      (position (posn 200 400))
      (sprite edge)
      (rotation 0))

    (door #:to rpg
      (physics-system 400 10
                      #:mass 1000000 ;Static?
                      )
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



