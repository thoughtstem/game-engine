#lang racket

(provide start-stop-animation
         key-animator-system
         player-info-closed?
         update-move-info
         show-move-info
         )

(require 2htdp/image
         posn)
(require "../game-entities.rkt")
(require "../components/animated-sprite.rkt")
(require "../components/backdrop.rkt")
(require "../components/key-movement.rkt")
(require "../components/counter.rkt")
(require "../components/direction.rkt")
(require "../components/rotation-style.rkt")
(require "../components/lock-to.rkt")
(require "../components/on-key.rkt")
(require "../components/observe-change.rkt")
(require "../components/spawn-once.rkt")
(require "../components/on-rule.rkt")
(require "../entity-helpers/sprite-util.rkt")
(require "../entity-helpers/dialog-util.rkt")

(define (start-stop-animation g e1 e2)
  (if (moving? g e2)
      ((start-animation) g e2)
      ((stop-animation) g e2)))

(define (key-animator-system)
  (list (direction 0)
        (on-key 'right (set-direction 0))
        (on-key 'left  (set-direction 180))
        (observe-change moving? start-stop-animation)
        (rotation-style 'left-right)
        
        )) 

(define (player-info-closed? g e)
  (not (get-entity "player info" g)))

(define (update-move-info)
  (lambda (g e)
    (define pos (get-component e posn?))
    (define pos-x (exact-floor (posn-x pos)))
    (define pos-y (exact-floor (posn-y pos)))
    (define current-tile (game->current-tile g))
    (define hue (get-hue-val e))
    (define size (get-size-val e))
    (define info-img (draw-dialog (~a "(posn " pos-x " " pos-y ")"
                                     "\nTile: " current-tile
                                     (if hue  (~a "\nHue:  " (modulo hue 360)) "")
                                     (if size (~a "\nSize: " size) ""))))
    ((change-sprite (new-sprite info-img)) g e)))
   
(define (show-move-info g e)
  (define pos (get-component e posn?))
  (define pos-x (exact-floor (posn-x pos)))
  (define pos-y (exact-floor (posn-y pos)))
  (define current-tile (game->current-tile g))
  (define height (image-height (render (get-component e animated-sprite?))))
  (define hue (get-hue-val e))
  (define size (get-size-val e))
  (define info-entity
    (sprite->entity (draw-dialog (~a "(posn " pos-x " " pos-y ")"
                                     "\nTile: " current-tile
                                     (if hue  (~a "\nHue:  " (modulo hue 360)) "")
                                     (if size (~a "\nSize: " size) "")))
                    #:position (posn 0 (+ 10 (/ height 2)))
                    #:name     "player info"
                    #:components (static)
                                 (lock-to "player" #:offset (posn 0 (+ 10 (/ height 2))))
                                 (on-key "o" die)
                                 (on-rule player-is-moving? (update-move-info))))
  (add-component e (spawn-once info-entity)))