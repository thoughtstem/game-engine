#lang racket

(provide get-sprites-with-top
         draw-sprite
         draw-entity
         draw-entities
         draw-game)

(require 2htdp/image
         posn
         threading)
(require "../game-entities.rkt")
(require "../components/animated-sprite.rkt")
(require "../components/storage.rkt")
(require "../entity-helpers/rgb-hsb.rkt")


(define (get-sprites-with-top e)
  (define all-as (get-components e animated-sprite?))
  (define top-posn (if (get-storage "Top" e)
                       (get-component (get-storage-data "Top" e) posn?)
                       (posn 0 0)))
  (define updated-top-sprites
    (if (get-storage "Top" e)
        (let ([top-sprites (get-components (get-storage-data "Top" e) animated-sprite?)])
          (map (compose (curry set-x-offset (posn-x top-posn))
                        (curry set-y-offset (posn-y top-posn))) top-sprites))
        '()))
  (append all-as updated-top-sprites))

; ==== Draw Functions for Documentation Only ====
(define/contract (draw-sprite s)
  (-> sprite? image?)
  (if (image? s)
      s
      (if (eq? (get-color s) 'black)
          (~> s
              (render _)
              (rotate (- (get-rotation s)) _))
          (~> s
              (render _)
              (rotate (- (get-rotation s)) _)
              (tint-img (get-color s) _))
          )))
                            
(define (draw-entity e)
  (define ss (reverse (get-sprites-with-top e)))

  (if (empty? ss)
      empty-image
      (overlay-sprites ss)))

(define/contract (overlay-sprites ss)
  (-> (listof animated-sprite?)
      image?)

  (define current-image
    (draw-sprite (first ss)))

  (if (= 1 (length ss))
      current-image
      (overlay/offset
       current-image
       (- (animated-sprite-x-offset (first ss)))
       (- (animated-sprite-y-offset (first ss)))
       (overlay-sprites (rest ss)))))

(define (draw-entities es)
  (if (= 1 (length es))
      (draw-entity (first es))
      (let* ([p (get-component (first es) posn?)]
             ;[p-top (if (get-storage "Top" (first es))
             ;           (get-component (get-storage-data "Top" (first es))
             [x (posn-x p)]
             [y (posn-y p)])
        (place-image (draw-entity (first es))
                     x y
                     (draw-entities (rest es))))))

(define/contract (draw-game g)
  (-> game? image?)
  (define (not-hidden e) (and (not (get-component e hidden?))
                              (not (get-component e disabled?))))
  (define not-hidden-entities (filter not-hidden (game-entities g)))
  (define regular-entities (filter not-ui? not-hidden-entities))
  (define ui-entities (filter ui? not-hidden-entities))
  (define entities (append ui-entities regular-entities))
  (draw-entities entities))

; =====================================================
