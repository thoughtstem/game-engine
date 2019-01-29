#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (except-out (struct-out detect-collide) detect-collide)
         (rename-out (new-detect-collide detect-collide)))

(component detect-collide (name1 name2 func))

; is make-detect-collide needed if there are no changes?
;(define (make-detect-collide name1 name2 func)
;  (new-detect-collide name1 name2 func)) 

(define (update-detect-collide g e c)
  (if (is-colliding-by-name? (detect-collide-name1 c) (detect-collide-name2 c) g)
      ((detect-collide-func c) g e)
      e))

(new-component detect-collide?
               update-detect-collide)
