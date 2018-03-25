#lang racket

(provide randomly-relocate-me)
(provide go-to-random)
(provide go-to)
;(provide random-size)
;(provide random-size-sprite)
(provide respawn)
(provide move-with-speed)
(provide move-random-speed)

(require "../game-entities.rkt")
(require "../components/direction.rkt")
(require "../components/every-tick.rkt")
(require "../ai.rkt")

(require posn)

(define WIDTH 640)
(define HEIGHT 480)

(define (randomly-relocate-me min-x max-x min-y max-y)
  (lambda (g e)
    (update-entity e posn? (posn (random min-x max-x)
                                 (random min-y max-y)))))

(define (go-to-random min-x max-x min-y max-y)
  (lambda (g e)
    (update-entity e posn? (posn (random min-x (add1 max-x))
                                 (random min-y (add1 max-y))))))

(define (go-to pos-x pos-y)
  (lambda (g e)
    (update-entity e posn? (posn pos-x pos-y))))

(define (respawn edge)
  (lambda (g e)
    ((cond
      [(eq? edge 'left)   (go-to 0 (random 0 HEIGHT))]
      [(eq? edge 'right)  (go-to WIDTH (random 0 HEIGHT))]
      [(eq? edge 'top)    (go-to (random 0 WIDTH) 0)]
      [(eq? edge 'bottom) (go-to (random 0 WIDTH) HEIGHT)])
      g e)))

(define (move-with-speed spd)
  (lambda (g e)
    (define dir (get-direction e));entity (get-name e) g)))
    (update-entity e every-tick?
                     (every-tick (move #:dir dir #:speed spd)))))

(define (move-random-speed min max)
  (lambda (g e)
    (define dir (get-direction e));(define dir (get-direction (get-entity (get-name e) g)))
    (update-entity e every-tick?
                     (every-tick (move #:dir dir #:speed (random min (add1 max)))))))