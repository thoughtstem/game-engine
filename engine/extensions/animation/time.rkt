#lang racket

(provide time-manager
         get-delta-time)

(require "../../core/main.rkt")

(require "./name.rkt")

(define-component time number?)
(define-component last-time number?)

(define (get-delta-time)
  (define t (get 'time-manager 'time))
  (define last-t (get 'time-manager 'last-time))

  (if (and t last-t)
    (- t last-t)
    0))

(define (time-manager)
  (entity
    (name 'time-manager)
    
    (last-time
      #f
      (if (get-time) (get-time) #f))

    (time
      #f
      (current-inexact-milliseconds))))









