#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (struct-out direction)
         get-direction)

(struct direction (dir))

(define (update-direction g e c) e)

(define (get-direction e)
  (direction-dir (get-component e direction?)))

(new-component direction?
               update-direction) 