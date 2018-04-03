#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (struct-out detect-collide))

(struct detect-collide (name1 name2 func))

(define (update-detect-collide g e c)
  (if (is-colliding-by-name? (detect-collide-name1 c) (detect-collide-name2 c) g)
      ((detect-collide-func c) g e)
      e))

(new-component detect-collide?
               update-detect-collide)
