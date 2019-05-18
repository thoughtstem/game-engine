#lang racket

(provide
  x
  y

  position
  get-position
  position?

  rotation
  get-rotation

  size
  get-size

  move-to)

(require "../../core/main.rkt" posn)

(define-component position posn?)
(define-component rotation number?)
(define-component size number?)                                         

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
