#lang racket

(require "../game-entities.rkt")
(require "./direction.rkt")
(require "./animated-sprite.rkt")
(require posn)
(require 2htdp/image)
(require threading)

(provide (struct-out rotation-style))

(struct rotation-style (mode))

(define (switch-animations-if-necessary c e)
  (define mode (rotation-style-mode c))
  (define dir (get-direction e))
  (define e-with-new-animation
    (cond
      [(eq? mode 'left-right)
       (cond
         [(and (< dir 270) (> dir 90))
          (update-entity e animated-sprite?
                         (curry set-x-scale -1))]
         [(and (or (> dir 270) (< dir 90)))
          (update-entity e animated-sprite?
                         (curry set-x-scale 1))]
         [else e])]
      [(eq? mode 'face-direction)
       (update-entity e animated-sprite?
                      (curry set-angle dir))
       ]))
  
  (update-entity e-with-new-animation rotation-style? c))

(define (update-rotation-style g e c)
  (switch-animations-if-necessary c e))

(new-component rotation-style?
               update-rotation-style) 