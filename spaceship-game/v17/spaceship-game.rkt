#lang racket

(require "../../game-engine.rkt"
         "../common/instructions.rkt"
         "../common/game-over-screen.rkt"
         "../assets/ore-sprite.rkt"
         "../assets/spaceship-sprite.rkt"
         "../assets/space-bg-generator.rkt")

(define WIDTH  640)
(define HEIGHT 480)

(define bg-entity
  (sprite->entity (space-bg-sprite WIDTH HEIGHT 100)
                  #:name     "bg"
                  #:position (posn 0 0)))

(define (wall-tile p)
  (sprite->entity (scale 0.5 (crop 0 0 62 62 (bitmap "./tiles.jpg"))) ;Move to asset helper...
                  #:name       "wall"
                  #:position   p
                  #:components (physical-collider)))

(define (spaceship-entity)
  (sprite->entity (sprite-map shrink spaceship-sprite)
                  #:name       "ship"
                  #:position   (posn 100 100)
                  #:components (physical-collider)
                               (key-movement 5)
                               (on-collide "ore"    (change-speed-by 10))
                               (on-collide "enemy"  die)
                               (on-collide "bullet" die)))

(define (ore-entity p)
  (sprite->entity (ore-sprite 0)
                  #:position   p
                  #:name       "ore"
                  ;#:components (on-collide "ship" die)
                  ))

(define shrink (λ(i) (scale 0.5 i)))

(define (enemy-entity p)
  (sprite->entity (sprite-map shrink (spaceship-animator 'left))
                  #:position    p
                  #:name        "enemy"
                  #:components  (spawner bullet 40)))

(define bullet
  (sprite->entity (new-sprite (list (circle 2 "solid" "red")
                                    (circle 2 "solid" "orange")
                                    (circle 2 "solid" "yellow")
                                    (circle 2 "solid" "orange")) 1)
                  #:position   (posn 100 100)
                  #:name       "bullet"
                  #:components (every-tick (move-left #:min 0 #:speed 4))
                               (after-time 100    die)  
                               (on-collide "ship" die)
                               (on-collide "wall" die)))


(define (lost? g e)
  (not (get-entity "ship" g)))

(define (won? g e) 
  (define speed (get-speed (get-entity "ship" g)))
  (>= speed 10))

(start-game (instructions WIDTH HEIGHT "Use arrow keys to move")
            (wall-tile (posn 200 200))
            (wall-tile (posn 232 200))
            (wall-tile (posn 264 200))
            (wall-tile (posn 264 168))
            (wall-tile (posn 264 136))
            (game-over-screen won? lost?)
            (spaceship-entity)
            (ore-entity (posn 400 400))
            (enemy-entity (posn 400 150))
            bg-entity)