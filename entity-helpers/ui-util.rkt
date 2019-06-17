#lang racket

(provide bordered-box-sprite
         toast-entity
         toast-system
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
         "../components/storage.rkt"
         "../ai.rkt"
         "./sprite-util.rkt"
         "./movement-util.rkt")

(require 2htdp/image
         posn
         threading)

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
                              #:duration [dur 15]
                              #:speed    [spd 3])
  (define color-symbol (if (string? color)
                           (string->symbol color)
                           color))
  (sprite->entity (list (new-sprite message #:color color-symbol)
                        (new-sprite message #:x-offset -1 #:y-offset 1 #:color 'black))
                  #:name       "player toast"
                  #:position   p
                  #:components (hidden)
                               (layer "ui")
                               
                               (direction 270)
                               (physical-collider)
                               (speed spd)
                               (on-start (do-many (random-direction 240 300)
                                                  (random-speed (sub1 spd) (add1 spd))
                                                  show))
                               (every-tick (do-many (move)
                                                    (scale-sprite 1.03)))
                               (after-time dur die)))

(define (toast-system message #:color [color "yellow"]
                              #:position [p (posn 0 -20)]
                              #:duration [dur 15]
                              #:speed    [spd 3])
  (define color-symbol (if (string? color)
                           (string->symbol color)
                           color))
  (define main-sprite (new-sprite message
                                  #:x-offset (posn-x p)
                                  #:y-offset (posn-y p)
                                  #:color color-symbol))
  (define shadow-sprite (new-sprite message
                                    #:x-offset (+ (posn-x p) -1)
                                    #:y-offset (+ (posn-y p)  1)
                                    #:color 'black))
  (define random-x (random -2 3))
  (define random-y (random (sub1 (- spd)) (+ 2 (- spd))))
  (define toast-id (random 1000000))
  (define tid-list (map component-id (list main-sprite shadow-sprite)))

  (define (do-toast-fx g e)
    (define current-main-sprite   (get-component e (λ(c) (eq? (component-id c) (first tid-list)))))
    (define current-shadow-sprite (get-component e (λ(c) (eq? (component-id c) (second tid-list)))))
    ;change x, y, and scale
    (define new-main-sprite (~> current-main-sprite
                                (change-x-offset random-x _)
                                (change-y-offset random-y _)
                                (scale-xy 1.03 _)))
    (define new-shadow-sprite (~> current-shadow-sprite
                                  (set-x-offset (+ (get-x-offset new-main-sprite) -1) _)
                                  (set-y-offset (+ (get-y-offset new-main-sprite) 1) _)
                                  (scale-xy 1.03 _)))
    (~> e
        (update-entity _ (curry component-eq? current-main-sprite) new-main-sprite)
        (update-entity _ (curry component-eq? current-shadow-sprite) new-shadow-sprite)))

  (define toast-fx-component (every-tick do-toast-fx))
  
  (define (remove-toast g e)
    (~> e
        (remove-components _ (or/c (λ(c) (eq? (component-id c) (first tid-list)))
                                   (λ(c) (eq? (component-id c) (second tid-list)))
                                   (curry component-eq? toast-fx-component)))
        (remove-storage (~a "toast-" toast-id) _)))

  (define toast-remove-component (after-time dur remove-toast))
  
  (list shadow-sprite 
        main-sprite
        (storage (~a "toast-" toast-id) (list main-sprite shadow-sprite toast-remove-component))
        toast-fx-component
        toast-remove-component))


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
                                   #:speed    [spd 0.8]
                                   #:scale    [scale 1.0])
  (define color-symbol (if (string? color)
                           (string->symbol color)
                           color))
  (sprite->entity (list (new-sprite message #:color color-symbol #:scale scale)
                        (new-sprite message #:x-offset -1 #:y-offset 1 #:color 'black #:scale scale))
                  #:name       "player toast"
                  #:position   (posn 0 0)
                  #:components (hidden)
                               (layer "ui")
                               (direction 270)
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

