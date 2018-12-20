#lang racket

(require "../game-entities.rkt")
(require "./direction.rkt")
(require "./animated-sprite.rkt")
(require posn)
(require 2htdp/image)
(require threading)

(provide (struct-out rotation-style)
         set-rotation-style
         horizontal-flip-sprite
         vertical-flip-sprite)

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
       ]
      [else e]))
  
  ;(update-entity e-with-new-animation rotation-style? c)
  e-with-new-animation
  )

(define (update-rotation-style g e c)
  (switch-animations-if-necessary c e))

(new-component rotation-style?
               update-rotation-style)

(define (get-rotation-style e)
  (rotation-style-mode (get-component e rotation-style?)))


; ==== HANDLERS ====
(define (set-rotation-style mode)
  (lambda (g e)
    ;(displayln (~a "Current rotation-style: " (get-rotation-style e)))
    ;(displayln (~a "Attempt rotation-style change: " mode))
    (update-entity e rotation-style? (rotation-style mode))))

(define (horizontal-flip-sprite)
  (lambda (g e)
    (define x-scale (get-x-scale (get-component e animated-sprite?)))
    (update-entity e animated-sprite?
                   (curry set-x-scale (- x-scale)))))

(define (vertical-flip-sprite)
  (lambda (g e)
    (define y-scale (get-y-scale (get-component e animated-sprite?)))
    (update-entity e animated-sprite?
                   (curry set-y-scale (- y-scale)))))