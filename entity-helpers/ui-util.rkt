#lang racket

(provide bordered-box-sprite
         toast-entity
         ;player-toast-entity ;must remove provides from base langauges first
         game-toast-entity)

(require "../game-entities.rkt"
         "../component-util.rkt"
         "../components/animated-sprite.rkt"
         "../components/direction.rkt"
         "../components/speed.rkt"
         "../components/on-start.rkt"
         "../components/every-tick.rkt"
         "../components/after-time.rkt"
         "../ai.rkt"
         "./sprite-util.rkt"
         "./movement-util.rkt")

(require 2htdp/image
         posn)

(define (bordered-box-sprite w h #:outer-border-color [outer-border-color 'black]
                                 #:border-color       [border-color 'white]
                                 #:color              [box-color 'dimgray])
  (define outer-border-img (square 1 'solid outer-border-color))
  (define inner-border-img (square 1 'solid border-color))
  (define box-img (square 1 'solid box-color))

  (precompile! outer-border-img
               inner-border-img
               box-img)
  
  (list (new-sprite  box-img
                     #:animate #f
                     #:x-scale (- w 6)
                     #:y-scale (- h 6))
        (new-sprite inner-border-img
                    #:animate #f
                    #:x-scale (- w 2)
                    #:y-scale (- h 2))
        (new-sprite outer-border-img
                    #:animate #f
                    #:x-scale w
                    #:y-scale h)
        ))

(define (toast-entity message #:color [color "yellow"]
                              #:position [p (posn 0 -20)]
                              #:duration [dur 25]
                              #:speed    [spd 3])
  (define color-symbol (if (string? color)
                           (string->symbol color)
                           color))
  (sprite->entity (new-sprite message #:x-offset -1 #:y-offset 1 #:color 'black)
                  #:name       "player toast"
                  #:position   p
                  #:components (hidden)
                               (layer "ui")
                               (new-sprite message #:color color-symbol)
                               (direction 270)
                               ;(physical-collider)
                               (speed spd)
                               (on-start (do-many (random-direction 240 300)
                                                  (random-speed (sub1 spd) (add1 spd))
                                                  show))
                               (every-tick (do-many (move)
                                                    (scale-sprite 1.03)))
                               (after-time dur die)))

(define (player-toast-entity message #:color [color "yellow"])
  (define color-symbol (if (string? color)
                           (string->symbol color)
                           color))
  (sprite->entity (new-sprite message #:x-offset -1 #:y-offset 1 #:color 'black)
                  #:name       "player toast"
                  #:position   (posn 0 0)
                  #:components (hidden)
                               (layer "ui")
                               (new-sprite message #:color color-symbol)
                               (direction 270)
                               ;(physical-collider)
                               (speed 3)
                               (on-start (do-many (go-to-entity "player" #:offset (posn 0 -20))
                                                  (random-direction 240 300)
                                                  (random-speed 2 4)
                                                  show))
                               (every-tick (do-many (move)
                                                    (scale-sprite 1.03)))
                               (after-time 15 die)))

(define (game-toast-entity message #:color    [color "yellow"]
                                   #:position [pos 'bottom]
                                   #:duration [dur 100]
                                   #:speed    [spd 0.8])
  (define color-symbol (if (string? color)
                           (string->symbol color)
                           color))
  (sprite->entity (list (new-sprite message #:color color-symbol)
                        (new-sprite message #:x-offset -1 #:y-offset 1 #:color 'black))
                  #:name       "player toast"
                  #:position   (posn 0 0)
                  #:components (hidden)
                               (layer "ui")
                               (direction 270)
                               ;(physical-collider)
                               (speed spd)
                               (on-start (do-many (cond [(eq? pos 'bottom) (go-to-pos 'bottom-center #:offset -32)]
                                                        [(eq? pos 'top)    (go-to-pos 'top-center    #:offset  32)]
                                                        [(eq? pos 'center) (go-to-pos 'center)]
                                                        [else              (go-to-pos 'bottom-center #:offset -32)])
                                                  ;(random-speed (sub1 spd) (add1 spd))
                                                  show))
                               (every-tick (do-many (move)
                                                    (random-direction)))
                               (after-time dur die)))

