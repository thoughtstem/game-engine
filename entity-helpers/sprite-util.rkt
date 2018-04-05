#lang racket

(provide set-size)
(provide scale-sprite)
(provide random-dec)
(provide random-size)

(require 2htdp/image)
(require "../game-entities.rkt")
(require "../components/animated-sprite.rkt")
;(require "../ai.rkt")

(require posn)

(define (set-size amount sprite)
  (lambda (g e)
    (define frames (animated-sprite-frames sprite))
    (define rate (animated-sprite-rate (get-component e animated-sprite?)))
    (define new-list (map (curry scale amount) (vector->list frames)))
    (define resized-sprite (new-sprite new-list rate))
    (define new-bb (image->bb (render resized-sprite)))
    (update-entity (update-entity e animated-sprite? (new-sprite new-list rate))
                   bb?
                   new-bb)))

(define (scale-sprite amount)
  (lambda (g e)
    (define frames (animated-sprite-frames (get-component e animated-sprite?)))
    (define rate (animated-sprite-rate (get-component e animated-sprite?)))
    (define new-list (map (curry scale amount) (vector->list frames)))
    (define resized-sprite (new-sprite new-list rate))
    (define new-bb (image->bb (render resized-sprite)))
    (update-entity (update-entity e animated-sprite? (new-sprite new-list rate))
                   bb?
                   new-bb)))

(define (random-dec min max)
  (define new-min (exact-round (* min 100)))
  (define new-max (exact-round (* max 100)))
  (/ (random new-min (add1 new-max)) 100))

(define (random-size min max sprite)
  (lambda (g e)
    (define frames (animated-sprite-frames sprite))
    (define rate   (animated-sprite-rate (get-component e animated-sprite?)))
    (define new-min (exact-round (* min 100)))
    (define new-max (exact-round (* max 100)))
    (define new-list (map (curry scale (/ (random new-min (add1 new-max)) 100)) (vector->list frames)))
    (define resized-sprite (new-sprite new-list rate))
    (define new-bb (image->bb (render resized-sprite)))
    (update-entity (update-entity e animated-sprite? (new-sprite new-list rate))
                   bb?
                   new-bb)))