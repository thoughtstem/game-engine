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
         draw-other-counter
         random-counter)

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

(define (random-counter min max)
  (lambda (g e)
     (update-entity e counter? (counter (random min (add1 max))))))

(new-component counter?
               update-counter)

; === SIMPLE STRUCTS ===
(struct hue-val  (hue))
(struct size-val (size))

(define (set-hue-val num)
 (lambda (g e)
     (update-entity e hue-val? (hue-val num))))

(define (get-hue-val e)
  (define hue-comp (get-component e hue-val?))
  (if hue-comp
      (hue-val-hue hue-comp)
      #f))

(define (change-hue-val-by inc)
  (lambda (g e)
    (define num (get-hue-val e))
    (update-entity e hue-val? (hue-val (+ num inc)))))

(define (set-size-val num)
  (lambda (g e)
    (update-entity e size-val? (size-val num))))

(define (get-size-val e)
  (define size-comp (get-component e size-val?))
  (if size-comp
      (size-val-size size-comp)
      #f))

(define (multiply-size-val-by inc)
  (lambda (g e)
    (define num (get-size-val e))
    (update-entity e size-val? (size-val (* num inc)))))

(provide (struct-out hue-val)
         (struct-out size-val)
         get-hue-val
         change-hue-val-by
         set-size-val
         get-size-val
         multiply-size-val-by)