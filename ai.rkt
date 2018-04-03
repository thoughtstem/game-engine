#lang racket

(provide move-up-and-down
         move-left
         move-right
         move-up
         move-down
         move-random
         move-dir-spd
         move
         spin)

(require posn)
(require "./game-entities.rkt")
(require 2htdp/image)
(require "./components/animated-sprite.rkt")
(require "./components/direction.rkt")
(require "./components/speed.rkt")

;A Lot of these could be implemented better (less stateful?)
;  Or go full state machine?
;  Waypoint system?

;Everything feels a bit cobbled together at the moment.

(define (move-up-and-down #:min min #:max max #:speed s)
  (define f (curry + s))
  (lambda (g e)
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (if (>= current-pos-y max)
        (set! f (curryr - s))
        (void))
    (if (<= current-pos-y min)
        (set! f (curry + s))
        (void))
    (update-entity e posn? (posn current-pos-x
                                 (f current-pos-y)))))

(define (move-left #:speed s)
  (lambda (g e)
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (update-entity e posn? (posn (- current-pos-x s)
                                 current-pos-y))))

(define (move-right #:speed s)
  (lambda (g e)
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (update-entity e posn? (posn (+ current-pos-x s)
                                 current-pos-y))))

(define (move-up #:speed s)
  (lambda (g e)
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (update-entity e posn? (posn current-pos-x
                                 (- current-pos-y s)))))

(define (move-down #:speed s)
  (lambda (g e)
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (update-entity e posn? (posn current-pos-x
                                 (+ current-pos-y s)))))

(define (move-random #:speed s)
  (define rx (* s (random -1 2)))
  (define ry (* s (random -1 2)))
  (lambda (g e)
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (update-entity e posn? (posn (+ rx current-pos-x)
                                 (+ ry current-pos-y)))))

(define (move-dir-spd #:dir d #:speed s)
  (lambda (g e)
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (define x-vel (* (cos (degrees->radians d)) s))
    (define y-vel (* (sin (degrees->radians d)) s))
    (update-entity e posn? (posn (+ current-pos-x x-vel)
                                 (+ current-pos-y y-vel)))))
(define (move)
  (lambda (g e)
    (define d (get-direction e))
    (define s (get-ai-speed e))
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (define x-vel (* (cos (degrees->radians d)) s))
    (define y-vel (* (sin (degrees->radians d)) s))
    (update-entity e posn? (posn (+ current-pos-x x-vel)
                                 (+ current-pos-y y-vel)))))

(define (spin #:speed s)
  (lambda (g e)
    (define f (λ(i) (rotate s i)))
    (update-entity e animated-sprite? (curry sprite-map f))))




