#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (struct-out counter)
         get-counter)

(struct counter (count))

(define (update-counter g e c) e)

(define (get-counter e)
  (counter-count (get-component e counter?)))

(new-component counter?
               update-counter) 