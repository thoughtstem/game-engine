#lang racket

(require "grids.rkt")

(provide ground-gen-uniform2)

(define (ground-gen-uniform2 w h base-sym second-sym prob)
  (define ret (add-noise second-sym prob
                         (grid-of base-sym w h)))
  ret)

(define (choose-between prob a b)
  (if (< (random) prob)
      a
      b))

(define (add-noise sym prob g)
  (grid-map (curry choose-between prob sym) g))

