#lang racket

(require "../../main.rkt"
         "./images.rkt"
         "./animations.rkt"
         2htdp/image)

(define-component speed number?)

;TODO: Handle physics memory leaks in switching worlds.

;TODO: Stop and doc the high level tutorial.
;  (Stretch goal: physics and doors)

;TODO: Avatar projectile -- just for more robust physics

;TODO: Different world tiles.  Infinite traversal?

;TODO: Move stuff to hero movement

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

(define-component test any/c)

(define (hero start)
  (entity
    (name 'avatar) 

    (position start
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

    ;TODO: Make animation helpers for this kind of thing.  But do we really want this effect in this game anyway?
    (transparency 0.5
                  (if (> (get-transparency) 0.99)
                    1
                    (+ 0.01 (get-transparency)))
                  

                  )
    (sprite bg-sprite)))

(define edge (register-sprite (rectangle 400 10 'solid 'red)))

(define (rpg (hero-start (posn 200 200)))
  (displayln "RPG start")
  (game 
    (input-manager) 
    (physics-manager)
    (time-manager)

    (hero hero-start)

    (door 
      #:to (thunk*
             (define p (get 'avatar 'position))
             (rpg (posn (posn-x p) 370)))
      #:detect (thunk* 
                 (get-physics-colliding?
                   (name=? 'avatar)))
      (physics-system 400 10
                      ;#:mass 1000000 ;TODO: Static?
                      #:static #t
                      )
      (position (posn 200 -10))
      (sprite edge)
      (rotation 0))

    (door 
      #:to (thunk*
             (define p (get 'avatar 'position))
             (rpg (posn 30 (posn-y p))))
      #:detect 
      (thunk* 
        (get-physics-colliding?
          (name=? 'avatar)))
      (physics-system 400 10
                      ;#:mass 1000000 ;Static?
                      #:static #t
                      )
      (position (posn 410 200))
      (sprite edge)
      (rotation (/ pi 2)))

    (door 
      #:to (thunk*
             (define p (get 'avatar 'position))
             (rpg (posn (posn-x p)
                        30)))
      #:detect 
      (thunk* 
        (get-physics-colliding?
          (name=? 'avatar)))
      (physics-system 400 10
                      ;#:mass 1000000 ;Static?
                      #:static #t
                      )
      (position (posn 200 410))
      (sprite edge)
      (rotation 0))

    (door 
      #:to (thunk*
             (define p (get 'avatar 'position))
             (rpg (posn 370 (posn-y p))))
      #:detect 
      (thunk* 
        (get-physics-colliding?
          (name=? 'avatar)))
      (physics-system 400 10
                      ;#:mass 1000000 ;Static?
                      #:static #t
                      )
      (position (posn -10 200))
      (sprite edge)
      (rotation (/ pi 2)))

    (world)))

(play! 
  (game
    (door-manager 
      (rpg (posn 200 200)))))



