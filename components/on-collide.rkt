#lang racket

(require "../game-entities.rkt")
(require posn)

#;(
(provide (struct-out on-collide))

(struct on-collide (name func))

(define (update-on-collide g e c)
  (if (is-colliding-with? (on-collide-name c) g e)
      ((on-collide-func c) g e)
      e))

(new-component on-collide?
               update-on-collide)

)