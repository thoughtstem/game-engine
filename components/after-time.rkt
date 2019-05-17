#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (rename-out (make-after-time after-time))
         (except-out (struct-out after-time) after-time)
         do-after-time
         set-after-time-delay)

(component after-time (accum speed func))

(define (make-after-time ticks func)
  (new-after-time 0 ticks func))

(define (inc-after-time a)
  (struct-copy after-time a
               [accum (add1 (after-time-accum a))]))

(define (after-time-ready? a)
  (>= (after-time-accum a)
      (after-time-speed a)))

(define (set-after-time-delay a delay)
  (struct-copy after-time a
               [speed delay]))

(define (update-after-time g e c)
  (if (after-time-ready? c)
      (remove-component ((after-time-func c) g e) (is-component? c))
      (update-entity    e                         (is-component? c) inc-after-time)))

(new-component after-time?
               update-after-time)

(define (do-after-time time f)
  (lambda (g e)
    (add-component e (make-after-time time f))))