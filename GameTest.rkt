#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(require threading)

(require "grids.rkt")
(require "sheets.rkt")
(require "marching-squares.rkt")


(define WIDTH 10)
(define HEIGHT 10)

(define GROUND_TILES (bitmap "./ground-tiles.png"))

(define GROUND_TILE_SIZE 32)

(define (tile x y)
  (crop (* x GROUND_TILE_SIZE)
        (* y GROUND_TILE_SIZE)
        GROUND_TILE_SIZE
        GROUND_TILE_SIZE
        GROUND_TILES))

(define (ground)
  (add-noise 's 0.5
             (grid-of 'g WIDTH HEIGHT)))


(define big-ground (ground))

(render-ground big-ground)
grass-stone-sheet
(merge-tiles (render2 big-ground))


(define (paint s x y event)
  (if (string=? event "button-down")
      (grid-set s
                (floor (/ y GROUND_TILE_SIZE))
                (floor (/ x GROUND_TILE_SIZE)) 's)
      s))

(define (draw g)
  (merge-tiles (render2 g)))

(merge-tiles
 (render2
  (big-bang big-ground
            (to-draw draw)
            (on-mouse paint))))















