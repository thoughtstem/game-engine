#lang racket

(require "../../main.rkt"
         "./images.rkt"
         "./animations.rkt"
         "./movement.rkt"
         "./world.rkt"
         2htdp/image)

;TODO: Having 64 sensors seems to slow things down too much.  Can we optimize again?
;  On the bright side, it seems to be the physics system for sure.  Can probably refactor it.

;TODO: Figure out why begin/separate is weird in chipmunk
; If we can get chipmunk working flawlessly, there's a TOOON of cool stuff we can do...  All sorts of games...

;TODO: Pick a random physics feature and implement it.  Like candy -- just having joint examples would be fucking sick.

;TODO: Handle physics memory leaks in switching worlds.

;TODO: Stop and doc the high level tutorial.
;  (Stretch goal: physics and doors)

;TODO: Avatar projectile -- just for more robust physics




;TODO: Continue building out the traversal mechanics.  Different world tiles.  
;  Infinite traversal?  Marching cubes?  Perlin noise?
;  These would all be fun to document in a tutorial
;Current issue: Can't move beyond the initial background sprite -- black sea...

;Can we do some fancy grids and marching squares here?

(define temp-green (register-sprite (square 48 'solid 'green)))
(define temp-red (register-sprite (square 48 'solid 'red)))


(define (world bg-sprite)
  (list
    (entity-grid 400 400 50
                 (thunk
                   (list 
                     ;Okay, this is slowing things down when there are 84 entities -- which shouldn't be that bad.  Gotta optimize.
                     (physics-system 50 50
                                     #:static #t
                                     #:sensor #t)
                     (transparency 1)
                     (sprite temp-green
                             (if (get-physics-colliding? 
                                   (name=? 'avatar)) 
                               temp-red 
                               temp-green)))))

    (entity
      (position (posn 0 0)
                (posn-scale -400
                            (get-tile-world-coord)))

      ;TODO: Make animation helpers for this kind of thing.  But do we really want this effect in this game anyway?
      (transparency 0.5
                    (if (> (get-transparency) 0.99)
                      1
                      (+ 0.01 (get-transparency))))

      ;Replace with an also-render subgame with many smaller tiles...
      (sprite bg-sprite))

    ))


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

(require "../../hotswap.rkt")

(no-hotswap me
    (game
      (door-manager 
        (tile))))




