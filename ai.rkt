#lang racket

(provide move-up-and-down
         move-left
         move-random)

(require posn)
(require "./game-entities.rkt")

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

(define (move-left #:min min #:speed s)
  (lambda (g e)
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (update-entity e posn? (posn (- current-pos-x s)
                                 current-pos-y))))


(define (move-random #:speed s)
  (lambda (g e)
    (define rx (* s (random -10 10)))
    (define ry (* s (random -10 10)))
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (update-entity e posn? (posn (+ rx current-pos-x)
                                 (+ ry current-pos-y)))))




