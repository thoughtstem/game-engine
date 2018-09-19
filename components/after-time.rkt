#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (rename-out (make-after-time after-time))
         do-after-time)

(struct after-time (accum speed func))

(define (make-after-time ticks func)
  (after-time 0 ticks func))

(define (inc-after-time a)
  (struct-copy after-time a
               [accum (add1 (after-time-accum a))]))

(define (after-time-ready? a)
  (>= (after-time-accum a)
      (after-time-speed a)))

(define (update-after-time g e c)
  (if (after-time-ready? c)
      (remove-component ((after-time-func c) g e) after-time?)
      (update-entity    e                         after-time? inc-after-time)))

(new-component after-time?
               update-after-time)

(define (do-after-time time f)
  (lambda (g e)
    (add-component e (make-after-time time f))))