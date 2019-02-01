#lang racket

(require "../game-entities.rkt")
(require "./direction.rkt")
(require posn)

(provide (rename-out (make-follow follow))
         (except-out (struct-out follow) follow))

;(provide (struct-out do-every))

(component follow (accum name speed))

(define (make-follow name [ticks 1])
  (new-follow 0 name ticks))

(define (reset-follow a)
  (struct-copy follow a
               [accum 0]))

(define (inc-follow a)
  (struct-copy follow a
               [accum (add1 (follow-accum a))]))

(define (follow-ready? a)
  (>= (follow-accum a)
      (follow-speed a)))

(define (update-follow g e c)
  (define target? (get-entity (follow-name c) g))
  (define target-x (unless (eq? target? #f) (posn-x (get-component target? posn?))))
  (define target-y (unless (eq? target? #f) (posn-y (get-component target? posn?))))
  (define x (posn-x (get-component e posn?)))
  (define y (posn-y (get-component e posn?)))
  (define new-dir (unless (eq? target? #f)(radians->degrees (atan (- target-y y) (- target-x x)))))
  (cond
    [(and (follow-ready? c) target?)
     (update-entity (update-entity e direction? (direction (if (negative? new-dir)
                                                 (+ 360 new-dir)
                                                 new-dir)))
                    follow? reset-follow)]
    [(and (not (follow-ready? c)) target?)
     (update-entity e                     follow? inc-follow)]
    [else e]))

(new-component follow?
               update-follow)

