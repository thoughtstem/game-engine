#lang racket

(require game-engine/main
         game-engine/spaceship-game/common/instructions)

(define WIDTH  640)
(define HEIGHT 480)

(define bg-entity
  (sprite->entity (rectangle WIDTH HEIGHT "solid" "black")
                  #:name     "bg"
                  #:position (posn 0 0)))

(define spaceship-entity
  (sprite->entity (list (circle 20 "solid" (make-color 250 0 0 255))
                        (circle 15 "solid" (make-color 200 50 0 255))
                        (circle 10 "solid" (make-color 150 100 0 255))
                        (circle 5 "solid"  (make-color 100 150 0 255))
                        (circle 10 "solid" (make-color 150 100 0 255))
                        (circle 15 "solid" (make-color 200 50 0 255)))
                  #:name       "ship"
                  #:position   (posn 100 100)
                  #:components (key-movement 5)))


(start-game (instructions WIDTH HEIGHT "Use arrow keys to move")
            spaceship-entity
            bg-entity)
  