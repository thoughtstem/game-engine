#lang racket

(provide space-bg-sprite)

(require "../../game-engine.rkt")

(define (space-bg-sprite w h n)
  (define ps (map list (map (thunk* (random w)) (range n))
                       (map (thunk* (random h)) (range n))))
  (new-sprite
   (list (stars w h ps)
         (stars w h ps)
         (stars w h ps))
   1))

(define (stars w h ps)
  (if (empty? ps)
      (rectangle w h "solid" "black")
      (place-image
       (rotate
        (random 45)
        (star 2 "solid" (make-color 255 255 255 (random 100 255))))
       (first (first ps)) (second (first ps))
       (stars w h (rest ps)))))
