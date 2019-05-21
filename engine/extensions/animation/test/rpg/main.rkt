#lang racket

(require "../../main.rkt"
         "./images.rkt"
         "./animations.rkt"
         2htdp/image)

(define-component speed number?)

;TODO: Try to detect collisions?  Print something in physics system on collide...

(define (opposite-signs a b)
  (cond
    [(and (negative? a) (positive? b)) #t]
    [(and (positive? a) (negative? b)) #t]
    [else #f]))

(define (thruster posn-? )
  (define p (as-posn (get-current-input)))
  (define ?-dir (posn-? p))

  (define v? (if (not (get-velocity)) 
               0
               (posn-? (get-velocity))))

  (define maxv 200)

  (cond 
    [(and (not (= 0 ?-dir)) ;Want to move
          (<= (abs v?) maxv))      ;Not going too fast..
      (* 100 (get-delta-time) ?-dir)]

    [(= 0 ?-dir) (- (* 10 v?))]
    [(opposite-signs v? ?-dir) (- (* 10 v?))]
    [else 0]))

(define (hero)
  (entity
    (name 'avatar) 

    (position (posn 200 200)
              ;TODO: Drifting (spaceship physics).  Make it cap out when max-velocity is reached.  Set velocity to 0 when no input?
              (get-physics-position))

    (rotation 0
              (get-physics-rotation))

    (physics-system 20 20
                    #:mass 1

                    #:forces 
                    (thunk* 
                      (posn
                        (thruster posn-x) 
                        (thruster posn-y))))

    (hero-animation)))

(define (world)
  (entity
    (position (posn 0 0))
    (sprite bg-sprite)))

(define edge (register-sprite (rectangle 400 10 'solid 'red)))

(define (rpg)
  (displayln "RPG start")
  (game 
    (input-manager) 
    (physics-manager)
    (time-manager)

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

(play! 
  (game
    (door-manager (rpg))))



