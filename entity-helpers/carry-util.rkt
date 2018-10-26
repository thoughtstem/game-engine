#lang racket

(provide movable
         nearest-to-player?
         near-player?
         get-carry-offset-x
         get-carry-offset-y
         carried?
         carried-by)

(require "../game-entities.rkt"
         "../components/backdrop.rkt"
         "../components/lock-to.rkt"
         "../entity-helpers/movement-util.rkt"
         (only-in "../entity-helpers/dialog-util.rkt"
                  draw-dialog)
         "../components/animated-sprite.rkt"
         "../components/on-key.rkt"
         "../components/observe-change.rkt"
         "../components/spawn-once.rkt"
         "../components/counter.rkt"
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
  (define player (entity-with-name "player" g))
  (define e-width  (image-width  (render (get-component e animated-sprite?))))
  ;(define e-height (image-height (render (get-component e animated-sprite?))))
  (define p-width  (image-width  (render (get-component player animated-sprite?))))
  ;(define p-height (image-height (render (get-component player animated-sprite?))))
  (define range (+ (/ e-width 2) (/ p-width 2) 10))
  ((near-entity? "player" range) g e))

(define (nearest-to-player? g e)
  (define all-es (game-entities g) #;(filter (has-component? carriable?)
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

  #;(displayln (list (get-name e) (get-id e) my-dist other-distances))

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

(define (display-entity e)
  (define i (draw-entity e))
  (define p (get-posn e))
  (define a (get-component e active-on-bg?))
  
  (displayln i)
  (displayln (~a "(posn "
                 (exact-round (posn-x p))
                 " "
                 (exact-round (posn-y p))
                 ")"))
  (displayln (~a "(active-on-bg "
                 (first (active-on-bg-bg-list a))
                 ")"))
  
  e)

(define (draw-info g e1 e2)
  (define e2-pos (get-component e2 posn?))
  (define pos-x (exact-floor (posn-x e2-pos)))
  (define pos-y (exact-floor (posn-y e2-pos)))
  (define current-tile (game->current-tile g))
  (define e2-height (image-height (render (get-component e2 animated-sprite?))))
  (define hue (get-hue-val e2))
  (define size (get-size-val e2))
  (define info-entity
    (sprite->entity (draw-dialog (~a "(posn " pos-x " " pos-y ")"
                                     "\nTile: " current-tile
                                     (if hue  (~a "\nHue:  " (modulo hue 360)) "")
                                     (if size (~a "\nSize: " size) "")))
                    #:position (posn 0 (+ 10 (/ e2-height 2)))
                    #:name     "info"
                    #:components (static)
                                 (active-on-bg current-tile)
                                 (on-key "z" die)))
  (if (carried? g e2)
      (begin
        (displayln "Picked up")
        e2)
      (begin (displayln "Dropped")
             (if (void? e1)
                 e2
                 (add-component e2 (spawn-once info-entity))))
      ))

; === GENERIC SYSTEM ===
; TODO: only carry one at a time option
;       calculate offset from game entities, use struct?
(define (movable #:carry-offset   [offset (posn 0 0)]
                 #:storable-items [movable-item-list #f]
                 #:pickup-key     [pickup-key "z"]
                 #:drop-key       [drop-key "x"]
                 #:pickup-sound   [pickup-sound #f]
                 #:drop-sound     [drop-sound    #f]
                 #:show-info?     [show-info? #f]
                 #:on-drop        [on-drop display-entity])
  (list (carriable)
        (on-key pickup-key #:rule (and/r nearest-to-player?
                                         near-player?
                                         (not/r carried?)
                                         (not/r (other-entity-locked-to? "player")))
                (add-lock-to "player" #:offset offset))
        (on-key drop-key #:rule carried? (remove-lock-to))
        (observe-change carried? 
                        (λ(g e1 e2)
                          (if (carried? g e2)
                              (begin
                                (remove-component e2 active-on-bg?))
                              (on-drop
                               (if (void? e1)
                                   e2
                                   (add-component e2 (active-on-bg (game->current-tile g))))
                               ))  ))
        (if show-info?
            (observe-change carried? draw-info)
            #f)))


(define (carried-by g e)
  (define target-name (lock-to-name (get-component e lock-to?)))

  (entity-with-name target-name g))

