#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (rename-out [new-every-tick every-tick])
         (except-out (struct-out every-tick) every-tick))

(component every-tick (func))

(define (update-every-tick g e c)
  ((every-tick-func c) g e))

(new-component every-tick?
               update-every-tick)  