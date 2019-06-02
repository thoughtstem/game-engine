#lang racket

(require "../../main.rkt"
         "./images.rkt"
         "./animations.rkt"
         "./movement.rkt"
         "./world.rkt"
         2htdp/image)

;START USING HOTSWAP

;TODO: Having 64 sensors seems to slow things down too much.  Can we optimize again?
;  On the bright side, it seems to be the physics system for sure.  Can probably refactor it.
;  Profiler tells me it's sub-games and physics-systems that are the biggest offenders in this system.
;  Which sub-games?
;     Hmmm.  I wonder if that's because things are nested under them...  There's a heirarchy here... need a better way of displaying performance data.  Some kind of tree map... Or divide things by the number of that type of component...
;
;  When I make everything in physics-system inert, it gets much faster -- but it still isn't at full.  I feel like 84 entities with 10 inert components shouldn't have that much of an effect...
;      So two prongs:
;        1) Can we speed up the tick in general?
;        2) Can we speed up physics system?

;  Bench:  30s-40s with inert components, 40s-50s if no update-physics-system, and 50s-60s if no grid entities

; See ../grid.rkt example as a benchmark.  


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
                     (transparency 0.5)
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

(play!
    (game
      (door-manager 
        (tile))))



