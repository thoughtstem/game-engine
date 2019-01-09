#lang racket

(provide change-sprite)
(provide set-size)
(provide scale-sprite)
(provide rotate-sprite)
(provide random-dec)
(provide random-size)
(provide set-color)
(provide change-color-by)
(provide random-color)
(provide random-tint)
(provide spawn)
(provide open-dialog
         hide
         show
         start-animation
         stop-animation)

(provide (all-from-out "./rgb-hsb.rkt"))

(require 2htdp/image)
(require "../game-entities.rkt")
(require "../components/animated-sprite.rkt")
(require "../components/direction.rkt")
(require "../components/rotation-style.rkt")
(require "../components/spawn-once.rkt")
(require "../components/spawn-dialog.rkt")
(require "./rgb-hsb.rkt")
(require "../components/after-time.rkt")
;(require "../ai.rkt")

(require posn
         threading)

(define (change-sprite sprite-or-func)
  (lambda (g e)
    (define sprite (if (procedure? sprite-or-func)
                       (sprite-or-func)
                       sprite-or-func))
    (define new-bb (image->bb (render sprite)))
    (update-entity (update-entity e animated-sprite? sprite)
                   bb?
                   new-bb)))

(define (set-size amount)
  (lambda (g e)
    (update-entity e animated-sprite?
                   (curry set-scale-xy amount))))

(define (scale-sprite amount #:for [d #f])
  (lambda (g e)
    (define all-sprites (get-components e animated-sprite?))
    (define original-sprites (map (λ (as) (struct-copy animated-sprite as)) all-sprites))
    
    (define (revert-back g e)
      (~> e
          (remove-components _ animated-sprite?)
          (add-components _ original-sprites))
      )

    (define (scale-a-sprite as)
      (define xs (get-x-scale as))
      (define ys (get-y-scale as))
      (define xo (get-x-offset as))
      (define yo (get-y-offset as))
      (struct-copy animated-sprite as
                   [x-scale (* xs amount)]
                   [y-scale (* ys amount)]
                   [x-offset (* xo amount)]
                   [y-offset (* yo amount)]))

    (define new-sprites (map scale-a-sprite original-sprites))
    
    (~> e
        (remove-components _ animated-sprite?)
        (add-components _ new-sprites)
        (add-components _ (if d
                              (after-time d revert-back)
                              #f)))))

(define (rotate-sprite amount #:for [d #f])
  (lambda (g e)
    (define all-sprites (get-components e animated-sprite?))
    (define original-sprites (map (λ (as) (struct-copy animated-sprite as)) all-sprites))
    
    (define (revert-back g e)
      (~> e
          (remove-components _ animated-sprite?)
          (add-components _ original-sprites))
      )

    (define (rotate-a-sprite as)
      (define rot (get-rotation as))
      (struct-copy animated-sprite as
                   [rotation (degrees->radians (+ rot amount))]))

    (define new-sprites (map rotate-a-sprite original-sprites))
    
    (~> e
        (remove-components _ animated-sprite?)
        (add-components _ new-sprites)
        (add-components _ (if d
                              (after-time d revert-back)
                              #f)))))

(define (random-dec min max)
  (define new-min (exact-round (* min 100)))
  (define new-max (exact-round (* max 100)))
  (/ (random new-min (add1 new-max)) 100))

(define (random-size min max)
  (lambda (g e)
    (update-entity e animated-sprite?
                   (curry set-scale-xy (+ min (* (random) (- max min)))))))

;This is broken...
;Not broken anymore, but recompiles each color change.
;todo: use mode lambda for color changing
(define (change-color-by amount)
  (lambda (g e)
    (define s (get-component e animated-sprite?))
    (define frames (animated-sprite-o-frames s))
    (define (ensure-image image-or-fast-image)
      (if (fast-image? image-or-fast-image)
          (fast-image-data image-or-fast-image)
          image-or-fast-image))
    (define image-frames (map ensure-image (vector->list frames)))
    (define new-list (map fast-image (map (curry change-img-hue amount) image-frames)))
    (update-entity e animated-sprite? (struct-copy animated-sprite s
                                                   [frames   (list->vector new-list)]
                                                   [o-frames (list->vector new-list)]
                                                   ))))

(define (set-color amount)
  (lambda (g e)
    (define s (get-component e animated-sprite?))
    (define frames (animated-sprite-o-frames s))
    (define new-list (map (curry change-img-hue amount) (vector->list frames)))
    (update-entity e animated-sprite?
                   (struct-copy animated-sprite s [frames (list->vector new-list)]))))

(define (random-color min max)
  (lambda (g e)
    (define s (get-component e animated-sprite?))
    (define frames (animated-sprite-o-frames s))
    (define hue-change (random min max))
    (define new-list (map (curry change-img-hue hue-change) (vector->list frames)))
    (update-entity e animated-sprite? (struct-copy animated-sprite s [frames (list->vector new-list)]))))

(define (random-tint)
  (lambda (g e)
    (define s (get-component e animated-sprite?))
    (define frames (animated-sprite-o-frames s))
    (define random-color (make-color-hue (random 255) 255))
    (define new-list (map (curry tint-img random-color) (vector->list frames)))
    (update-entity e animated-sprite? (struct-copy animated-sprite s [frames (list->vector new-list)]))))

(define (spawn s #:relative? [relative? #t]) 
  (lambda (g e)
    (add-component e (spawn-once s #:relative? relative?))))

(define (open-dialog s) 
  (lambda (g e)
    (add-component e (spawn-dialog s))))

(define (hide g e)
  (add-component (remove-component e hidden?) (hidden)))

(define (show g e)
  (remove-component e hidden?))

(define (start-animation)
  (lambda (g e)
    ;(displayln (~a (get-name e) ": STARTING ANIMATION"))
    (define as (get-component e animated-sprite?))
    (update-entity e
                   animated-sprite?
                   (struct-copy animated-sprite as
                                [animate? #t]))))

(define (stop-animation)
  (lambda (g e)
    ;(displayln (~a (get-name e) ": STOPPING ANIMATION"))
    (define as (get-component e animated-sprite?))
    (update-entity e
                   animated-sprite?
                   (struct-copy animated-sprite as
                                [current-frame 0]
                                [ticks 0]
                                [animate? #f]))))

