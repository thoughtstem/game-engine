#lang racket

(require "../../game-engine.rkt"
         "../common/instructions.rkt")

(define WIDTH  640)
(define HEIGHT 480)

(define bg-entity
  (sprite->entity (rectangle WIDTH HEIGHT "solid" "black")
                  #:name     "bg"
                  #:position (posn 0 0)))

(define spaceship-entity
  (sprite->entity (circle 20 "solid" "red")
                  #:name       "ship"
                  #:position   (posn 100 100)
                  #:components (key-movement 5)))

(start-game (instructions WIDTH HEIGHT "Use arrow keys to move")
            spaceship-entity
            bg-entity)
 
  