#lang racket

(require "../../main.rkt"
         "./images.rkt"
         "./animations.rkt"
         "./movement.rkt"
         "./world.rkt"
         2htdp/image)

(define-component speed number?)

;TODO: Handle physics memory leaks in switching worlds.

;TODO: Stop and doc the high level tutorial.
;  (Stretch goal: physics and doors)

;TODO: Avatar projectile -- just for more robust physics

;TODO: Continue building out the traversal mechanics.  Different world tiles.  
;Infinite traversal?  Marching cubes?  Perlin noise?
;These would all be fun to document in a tutorial

(define (hero start)
  (entity
    (name 'avatar) 

    (position start
              (get-physics-position))

    (rotation 0
              (get-physics-rotation))

    (physical-movement)

    (hero-animation)))


(define edge (register-sprite (rectangle 400 10 'solid 'red)))

(define (avatar-wrap-around g)
  (define old-coord
    (get-tile-world-coord (CURRENT-GAME)))  

  (define new-coord
    (get-tile-world-coord g))

  (define travel-direction
    (posn-subtract new-coord old-coord))

  (define old-avatar-position
    (get-position
      (get-entity (CURRENT-GAME)
                  (name=? 'avatar))))

  (define new-avatar-position
    (posn-wrap 0 400  
      (posn-add
        old-avatar-position
        (posn-scale 30 travel-direction))))

  (update-entity g
                 (name=? 'avatar) 
                 (curryr set-position new-avatar-position)))


(define (tile (avatar (hero (posn 200 200))))
  (displayln "RPG start")
  (game 
    (input-manager) 
    (physics-manager)
    (time-manager)

    avatar 

    (world-edge-system 
      #:to tile
      #:transition avatar-wrap-around)
    (world bg-sprite)))

(play! 
  (game
    (door-manager 
      (tile))))



