#lang racket

(provide movable
         nearest-to-player?
         get-carry-offset-x
         get-carry-offset-y
         carried?)

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
         threading)

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
        (add-component (remove-component e physical-collider?)
                       (lock-to name #:offset offset)))))

(define (remove-lock-to)
  (lambda (g e)
    (add-component (remove-component e lock-to?) (physical-collider))))

(define (near-player? g e)
  ((near-entity? "player") g e))

(define (nearest-to-player? g e)
  (define all-es (filter (has-component? carriable?)
                         (game-entities g)))

  (define player (entity-with-name "player" g))

  (define all-but-me-and-player
    (~> all-es
        (remove player _ entity-eq?)
        (remove e      _ entity-eq?)))
  
  (define my-dist (distance-between (get-posn e)
                                    (get-posn player))) 

  (define other-distances (map (curry distance-between (get-posn player))
                               (map get-posn all-but-me-and-player)))

  (or (empty? other-distances)
      (< my-dist (apply min other-distances))))

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



(struct carriable ())

; === GENERIC SYSTEM ===
; TODO: only carry one at a time option
;       calculate offset from game entities, use struct?
(define (movable #:carry-offset   [offset (posn 0 0)]
                 #:storable-items [movable-item-list #f]
                 #:pickup-key     [pickup-key "z"]
                 #:drop-key       [drop-key "x"]
                 #:pickup-sound   [pickup-sound #f]
                 #:drop-sound     [drop-sound    #f])
  (list (carriable)
        (on-key pickup-key #:rule (and/r nearest-to-player?
                                         near-player?
                                         (not/r carried?)
                                         (not/r (other-entity-locked-to? "player")))
                (add-lock-to "player" #:offset offset))
        (on-key drop-key #:rule carried? (remove-lock-to))))




