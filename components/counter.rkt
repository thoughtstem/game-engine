#lang racket

(require "../game-entities.rkt")
(require "./animated-sprite.rkt")
(require 2htdp/image)

(require posn)
(provide (except-out (struct-out counter) counter)
         (rename-out [make-counter counter])
         set-counter
         get-counter
         change-counter-by
         draw-counter
         draw-other-counter
         random-counter)

(component counter (count))

(define (make-counter c)
  (new-counter c))

(define (update-counter g e c) e)

(define (set-counter num)
 (lambda (g e)
     (update-entity e counter? (struct-copy counter (get-component e counter?)
                                            [count num]))))

(define (get-counter e)
  (counter-count (get-component e counter?)))

(define (change-counter-by inc)
  (lambda (g e)
    (define num (get-counter e))
    (update-entity e counter?
                   (struct-copy counter (get-component e counter?)
                                [count (+ num inc)])
                   )))

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
     (update-entity e counter? (new-counter (random min (add1 max))))))

(new-component counter? update-counter)

; === SIMPLE STRUCTS ===
(component hue-val  (hue))
(component size-val (size))

;(define (make-hue-val hue)
;  (new-hue-val hue))

;(define (make-size-val size)
;  (new-hue-val size))

(define (set-hue-val num)
 (lambda (g e)
     (update-entity e hue-val? (new-hue-val num))))

(define (get-hue-val e)
  (define hue-comp (get-component e hue-val?))
  (if hue-comp
      (hue-val-hue hue-comp)
      #f))

(define (change-hue-val-by inc)
  (lambda (g e)
    (define num (get-hue-val e))
    (update-entity e hue-val? (new-hue-val (+ num inc)))))

(define (set-size-val num)
  (lambda (g e)
    (update-entity e size-val? (new-size-val num))))

(define (get-size-val e)
  (define size-comp (get-component e size-val?))
  (if size-comp
      (size-val-size size-comp)
      #f))

(define (multiply-size-val-by inc)
  (lambda (g e)
    (define num (get-size-val e))
    (update-entity e size-val? (new-size-val (* num inc)))))

(provide (except-out (struct-out hue-val) hue-val)
         (except-out (struct-out size-val) size-val)
         (rename-out (new-hue-val hue-val))
         (rename-out (new-size-val size-val))
         get-hue-val
         change-hue-val-by
         set-size-val
         get-size-val
         multiply-size-val-by)