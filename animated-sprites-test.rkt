#lang racket

(require 2htdp/image)
(require "./animated-sprites.rkt")

(require 2htdp/universe)
(require threading)

(define braid-guy
  (bitmap/url "https://cdn-images-1.medium.com/max/520/0*v-9KSDYtRcYIoCHv.png")
  )

(define monster
  (bitmap/url "https://cdn.tutsplus.com/mobile/uploads/legacy/Corona-SDK_Side-Scroller/3/monsterSpriteSheet.png"))


(define (pad i)
  (overlay i
           (square 60 "solid" "transparent")))

(define (c1 c) (pad (circle 10 "solid" c)))
(define (c2 c) (pad (circle 20 "solid" c)))
(define (c3 c) (pad (circle 30 "solid" c)))

(define (my-sprite c r )
  (new-sprite
    (list (c1 c)
          (c2 c)
          (c3 c))
    r))

(define braid-sprite
  (new-sprite
     (sheet->costume-list braid-guy 7 4 27)
     1))

(define monster-sprite
  (new-sprite
     (sheet->costume-list monster 7 1 6)
     1))

(define s1 (my-sprite "red"   5))
(define s2 (my-sprite "green" 10))
(define bg (square 200 "solid" "white"))

(define (draw sprites)
  (overlay
   (apply beside (map render sprites))
   bg))

(big-bang (list s1 s2 braid-sprite monster-sprite)
          (on-tick (curry map next-frame))
          (to-draw draw))
