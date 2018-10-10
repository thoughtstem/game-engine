#lang racket

(provide movable
         get-carry-offset-x
         get-carry-offset-y
         )

(require "../game-entities.rkt"
         "../components/backdrop.rkt"
         "../components/lock-to.rkt"
         "../components/active-on-bg.rkt"
         "../entity-helpers/movement-util.rkt"
         "../components/animated-sprite.rkt"
         "../components/on-key.rkt"
         "../component-util.rkt"
         posn
         2htdp/image
         )

; === GENENRIC RULES AND HELPERS ===
; TODO: add to game engine
(define (remove-active-on-bg)
  (lambda (g e)
    (remove-component e active-on-bg?)))

(define (add-active-on-bg)
  (lambda (g e)
    (define current-tile (get-current-tile (get-entity "bg" g)))
    (add-component e (active-on-bg current-tile))))

(define (add-lock-to name #:offset [offset (posn 10 0)])
  (lambda (g e)
    (if (get-component e lock-to?)
        e
        (add-component (remove-component e physical-collider?) (lock-to name #:offset offset)))))

(define (remove-lock-to)
  (lambda (g e)
    (add-component (remove-component e lock-to?) (physical-collider))))

(define (near-player-and-not-carried? g e)
    (and ((near-entity? "player") g e)
         (not (get-component e lock-to?))))

(define (carried? g e)
  (get-component e lock-to?))

;added optional location of a carried sprite: 'left or 'right(default)
(define (get-carry-offset-x player-sprite item-sprite
                                          #:item-location [item-loc 'right])
  (define p-img (render player-sprite))
  (define i-img (render item-sprite))
  (define pos-x (cond [(eq? item-loc 'right) (+ (/ (image-width p-img) 2) (/ (image-width i-img) 2))]
                      [(eq? item-loc 'left)  (- (+ (/ (image-width p-img) 2) (/ (image-width i-img) 2)))]))
  (define pos-y 0)
  (posn pos-x pos-y))

;added optional location for a carried sprite: 'top or 'bottom(default)
(define (get-carry-offset-y player-sprite item-sprite
                                          #:item-location [item-loc 'bottom])
  (define p-img (render player-sprite))
  (define i-img (render item-sprite))
  (define pos-x 0)
  (define pos-y (cond [(eq? item-loc 'bottom) (+ (/ (image-height p-img) 2) (/ (image-height i-img) 2))]
                      [(eq? item-loc 'top)    (- (+ (/ (image-height p-img) 2) (/ (image-height i-img) 2)))]))
  (posn pos-x pos-y))

; === GENERIC SYSTEM ===
; TODO: only carry on at a time option
;       calculate offset from game entities, use struct?
(define (movable #:carry-offset   [offset (posn 0 0)]
                 #:storable-items [movable-item-list #f]
                 #:pickup-key     [pickup-key "z"]
                 #:drop-key       [drop-key "x"]
                 #:pickup-sound   [pickup-sound #f]
                 #:drop-sound     [drop-sound    #f])
  (list (on-key pickup-key #:rule near-player-and-not-carried? (do-many (add-lock-to "player" #:offset offset)
                                                                        (remove-active-on-bg)))
        (on-key drop-key #:rule carried? (do-many (remove-lock-to)
                                                  (add-active-on-bg)))))




