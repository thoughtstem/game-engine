#lang racket

(require "../game-entities.rkt")
(require "./animated-sprite.rkt")
(require 2htdp/image)

(require posn)
(provide (struct-out counter)
         set-counter
         get-counter
         change-counter-by
         draw-counter
         draw-other-counter)

(struct counter (count))

(define (update-counter g e c) e)

(define (set-counter num)
 (lambda (g e)
     (update-entity e counter? (counter num))))

(define (get-counter e)
  (counter-count (get-component e counter?)))

(define (change-counter-by inc)
  (lambda (g e)
    (define num (get-counter e))
    (update-entity e counter? (counter (+ num inc)))))

(define (draw-counter label size color)
  (lambda (g e)
    (define count (get-counter e))
    (update-entity e animated-sprite? (new-sprite (text (~a label count) size color) 1))))

(define (draw-other-counter name label size color)
  (lambda (g e)
    (define count (get-counter (get-entity name g)))
    (update-entity e animated-sprite? (new-sprite (text (~a label count) size color) 1))))

(new-component counter?
               update-counter) 