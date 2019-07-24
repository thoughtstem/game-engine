#lang racket

(provide g)

(require game-engine
         2htdp/image)

(define (my-thing x y color)
  (entity
   (physics-system #:mass 1
                   20 20)
   (position (posn x y) (get-physics-position))
   (rotation 0 (get-physics-rotation))
   (sprite (register-sprite 
            (square 20 'solid color)))))

(define (random-thing)
  (my-thing
   (+ 200 (random -20 20))
   (+ 200 (random -20 20))
   (make-color (random 0 255) (random 0 255) 0)))

(define (bullet)
  (entity
   (physics-system #:mass 10
                   #:forces (thunk* (posn 10000 0))
                   20 20)
   (position (posn 10 200)
             (get-physics-position))
   (rotation 0 (get-physics-rotation))
   (sprite (register-sprite 
            (circle 10 'solid 'blue))
   )))

(define (wall x y)
  (entity
   (physics-system #:static #t
                   400 10)
   (position (posn x y))
   (sprite (register-sprite
            (rectangle 400 10 'solid 'white)))))

(define g
 (game 
  (physics-manager)
  (bullet)
  (wall 200 10)
  (wall 200 390)
  (map (thunk* (random-thing))
   (range 0 70))))

(module+ main
  (play! g))



