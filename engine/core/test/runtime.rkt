#lang racket

(require rackunit 
         "../main.rkt"
         "./util.rkt")

(define (uniq l)
  (remove-duplicates (filter identity l)))



(test-case "Id uniqueness after initialization" 
           (define e (entity (health 5)))
           (define g0 (init (game e e e)))
           (define g1 (init g0))
           (define g2 (tick g1))
           (define g3 (tick g2))


           ;All ids uniq from init onward?
           (ensure-uniqueness! g1)
           (ensure-uniqueness! g2) 
           (ensure-uniqueness! g3))




