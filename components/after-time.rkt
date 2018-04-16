#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (rename-out (make-after-time after-time)))

(struct after-time (accum speed func))

(define (make-after-time ticks func)
  (after-time 0 ticks func))

(define (reset-after-time a)
  (struct-copy after-time a
               [accum 0]))

(define (inc-after-time a)
  (struct-copy after-time a
               [accum (add1 (after-time-accum a))]))

(define (after-time-ready? a)
  (>= (after-time-accum a)
      (after-time-speed a)))

(define (update-after-time g e c)
  (if (after-time-ready? c)
      (update-entity ((after-time-func c) g e) after-time? reset-after-time)
      (update-entity e                     after-time? inc-after-time)))

(new-component after-time?
               update-after-time)

