#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (rename-out (make-do-every do-every)
                     (do-every      struct-do-every)
                     (do-every-rule struct-do-every-rule)
                     (do-every-func struct-do-every-func))
         do-every?)

(component do-every (accum speed rule func))

(define (make-do-every ticks #:rule [rule (lambda (g e) #t)] func)
  (new-do-every 0 ticks rule func))

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
  (if (and (do-every-ready? c)
           ((do-every-rule c) g e))
      (update-entity ((do-every-func c) g e) (is-component? c) reset-do-every)
      (update-entity e                       (is-component? c) inc-do-every)))

(new-component do-every?
               update-do-every)

