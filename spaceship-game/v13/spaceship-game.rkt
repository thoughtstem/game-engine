#lang racket

(require game-engine/main
         game-engine/spaceship-game/common/instructions
         game-engine/spaceship-game/common/game-over-screen
         game-engine/spaceship-game/assets/ore-sprite
         game-engine/spaceship-game/assets/spaceship-sprite
         game-engine/spaceship-game/assets/space-bg-generator)

(define WIDTH  800)
(define HEIGHT 800)

(define bg-entity
  (sprite->entity (space-bg-sprite WIDTH HEIGHT 100)
                  #:name     "bg"
                  #:position (posn 0 0)))

(define (spaceship-entity)
  (sprite->entity spaceship-sprite
                  #:name       "ship"
                  #:position   (posn 100 100)
                  #:components (key-movement 5)
                               (on-collide "ore"    (change-speed-by 1))
                               (on-collide "enemy"  die)
                               (on-collide "bullet" die)))

(define (ore-entity p)
  (sprite->entity (ore-sprite (random 10))
                  #:position   p
                  #:name       "ore"
                  #:components (on-collide "ship" (randomly-relocate-me 0 WIDTH 0 HEIGHT))))

(define (enemy-entity p)
  (sprite->entity (spaceship-animator 'left)
                  #:position    p
                  #:name        "enemy"
                  #:components  (every-tick (move-up-and-down #:min   0  
                                                              #:max   HEIGHT
                                                              #:speed 10))
                                (spawner bullet 20)))

(define bullet
  (sprite->entity (new-sprite (list (circle 5 "solid" "red")
                                    (circle 5 "solid" "orange")
                                    (circle 5 "solid" "yellow")
                                    (circle 5 "solid" "orange")) 1)
                  #:position   (posn 100 100)
                  #:name       "bullet"
                  #:components (every-tick (move-left #:min   0
                                                      #:speed 5))
                               (after-time 50     die)  
                               (on-collide "ship" die)))

(define (lost? g e)
  (not (get-entity "ship" g)))

(define (won? g e) 
  (define speed (get-speed (get-entity "ship" g)))
  (>= speed 10))

(start-game (instructions WIDTH HEIGHT "Use arrow keys to move")
            (game-over-screen won? lost?)
            (spaceship-entity)
            (ore-entity (posn 400 400))
            (enemy-entity (posn 500 300))
            (enemy-entity (posn 600 200))
            (enemy-entity (posn 700 100))
            bg-entity)
