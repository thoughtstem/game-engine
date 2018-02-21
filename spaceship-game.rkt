#lang racket

(require posn)
(require "game-entities.rkt")
(require "animated-sprites.rkt")
(require "sprite-machine.rkt")
(require "space-bg-generator.rkt")
(require 2htdp/image)
(require threading)

(define WIDTH  640)
(define HEIGHT 480)


;BACKGROUND

(define bg
  (~> (space-bg-sprite WIDTH HEIGHT 100)
      (sprite->entity _)
      (add-component _ (entity-name "bg"))))


;SPACESHIP

(define spaceship-anim
  (~> (bitmap/url "http://i.imgur.com/8zY5sBR.png")
      (sheet->costume-list _ 3 4 12)
      (drop _ 6)
      (take _ 3)
      (new-sprite _ 1)))

(define (make-spaceship p)
  (~> (sprite->entity spaceship-anim)
      (update-entity _ posn? p)
      (add-component _ (key-movement 5))
      (add-component _ (entity-name "ship"))))


;ORE

(define ore-sheet (bitmap/url "http://twicetwo.com/gallery/cgi/powerups.png"))

(define (ore-anim i)
  (~> ore-sheet
      (sheet->costume-list _ 8 20 (* 8 20))
      (drop _ (* i 8))
      (take _ 8)
      (new-sprite _ 1)))

(define (make-ore p)
  (~> (sprite->entity (ore-anim (random 10)))
      (update-entity _ posn? p)
      (add-component _ (on-collide maybe-respawn-ore))
      (add-component _ (entity-name "ore"))))

(define (maybe-respawn-ore g me)
  (define names (map get-name (colliding-with me g)))
  (if (member "ship" names)
      (randomly-relocate me)
      me))

(define (randomly-relocate e)
  (~> (posn (random WIDTH)
            (random HEIGHT))
      (make-ore _)))


;GAME

(define entities
  (list
   (make-spaceship (posn 100 400 ))
   (make-ore       (posn 200 400 ))
   bg))

(start-game entities)

