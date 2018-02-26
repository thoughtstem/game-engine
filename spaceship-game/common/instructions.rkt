#lang racket

(provide instructions)

(require "../../game-engine.rkt")

(define (screen w h msg color)
  (new-sprite (list (overlay (text msg 30 color)
                             (rectangle w h "solid" (make-color 0 0 0 100))))
              1))

(define (instructions w h msg (color "green"))
  (sprite->entity (screen w h msg color)
                  #:position   (posn (/ w 2)
                                     (/ h 2))
                  #:name       "ui"
                  #:components (after-time 0 50 die)))
