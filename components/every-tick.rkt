#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (rename-out [new-every-tick every-tick])
         every-tick?
         every-tick-func)

(component every-tick (func))

(define (update-every-tick g e c)
  ((every-tick-func c) g e))

(new-component every-tick?
               update-every-tick)  