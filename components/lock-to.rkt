#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (rename-out (make-lock-to lock-to))
         lock-to?)

(struct lock-to (name offset))

(define (make-lock-to name #:offset [offset (posn 0 0)])
  (lock-to name offset))

(define (update-lock-to g e c)
  (define target-pos (get-component (get-entity (lock-to-name c) g) posn?))
  (define offset-pos (lock-to-offset c))
  (define new-posn (posn (+ (posn-x target-pos) (posn-x offset-pos))
                         (+ (posn-y target-pos) (posn-y offset-pos))))
  (update-entity e posn? new-posn))

(new-component lock-to?
               update-lock-to)