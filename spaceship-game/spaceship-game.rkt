#lang racket

(require "../game-engine.rkt")

(define WIDTH  640)
(define HEIGHT 480)

;ASSETS

(define spaceship-sheet (bitmap/url "http://i.imgur.com/8zY5sBR.png"))
(define ore-sheet (bitmap/url "http://twicetwo.com/gallery/cgi/powerups.png"))
(define bg-sprite (space-bg-sprite WIDTH HEIGHT 100))

(define spaceship-sprite
  (sheet->sprite spaceship-sheet
                 #:rows        4
                 #:columns     3
                 #:row-number  3
                 #:speed       1))

(define (ore-sprite i)
  (sheet->sprite ore-sheet
                 #:rows        20
                 #:columns     8
                 #:row-number  (+ 1 i)
                 #:speed       1))

;ENTITIES

(define bg-entity
  (sprite->entity bg-sprite
                  #:position (posn 0 0)
                  #:name     "bg"))

(define (spaceship-entity p)
  (sprite->entity spaceship-sprite
                  #:position   p
                  #:name       "ship"
                  #:components (key-movement 5)))

(define (ore-entity p)
  (sprite->entity (ore-sprite (random 10))
                  #:position   p
                  #:name       "ore"
                  #:components (on-collide "ship" randomly-relocate-me)))

(define (randomly-relocate-me g e)
  (ore-entity (posn (random WIDTH)
                    (random HEIGHT))))

;GAME

(start-game (spaceship-entity (posn 100 400))
            (ore-entity       (posn 200 400))
            bg-entity)

