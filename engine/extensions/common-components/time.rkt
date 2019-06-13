#lang racket

(provide delta-time-entity
         get-delta-time)

(require "../../core/main.rkt")

(define-component time-then number?)
(define-component delta-time number?)

(define (delta-time-entity . cs)
  (entity
    (delta-time 0
                (/
                  (if (get-time-then) 
                    (- (current-inexact-milliseconds)  
                       (get-time-then))
                    0)
                  1000)) 

    cs

    (time-then #f (current-inexact-milliseconds))))



