#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (struct-out every-tick))

(struct every-tick (func))

(define (update-every-tick g e c)
  ((every-tick-func c) g e))

(new-component every-tick?
               update-every-tick)  