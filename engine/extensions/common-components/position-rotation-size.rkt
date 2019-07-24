#lang racket

(provide
  x
  y

  position
  set-position
  get-position
  position?

  rotation
  set-rotation
  get-rotation

  size
  get-size

  move-to
 
  posn-map
  posn-wrap
  posn-modulo)

(require "../../core/main.rkt" posn)

(define-component position posn?)
(define-component rotation number?)
(define-component size number?)                                         
(define (posn-map f p)
  (posn (f (posn-x p))
        (f (posn-y p))))

(define (posn-modulo p m)
  (posn-map
    (curryr modulo m)  
    p))

(define (posn-wrap b t p)
  (posn-map
    (curry number-wrap b t)
    p))

(define (number-wrap b t n)
  ;Works for most cases, but doesn't handle if n is more than (- t b) away from t or b.
  (cond 
    [(< n b) (- t (- b n))]
    [(> n t) (+ b (- n t))]
    [else n]))

(define (x e)
  (posn-x (get-position e)))

(define (y e)
  (posn-y (get-position e)))

(define (move-to p e)

  (define current-p 
    (get-component e 'position))

  (define new-p
    (set-position current-p p))

  (update-component e 
                    current-p
                    new-p))
