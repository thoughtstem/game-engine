#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (rename-out (make-do-every do-every)))

(struct do-every (accum speed func))

(define (make-do-every ticks func)
  (do-every 0 ticks func))

(define (reset-do-every a)
  (struct-copy do-every a
               [accum 0]))

(define (inc-do-every a)
  (struct-copy do-every a
               [accum (add1 (do-every-accum a))]))

(define (do-every-ready? a)
  (>= (do-every-accum a)
      (do-every-speed a)))

(define (update-do-every g e c)
  (if (do-every-ready? c)
      (update-entity ((do-every-func c) g e) do-every? reset-do-every)
      (update-entity e                     do-every? inc-do-every)))

(new-component do-every?
               update-do-every)

