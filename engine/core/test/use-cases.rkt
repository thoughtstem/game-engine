#lang racket

(require rackunit 
         "../main.rkt"
         "./util.rkt")

(require threading)

(begin ;test-case "Moving x and y"
  (define-component position  list?)
  (define-component direction list?)

  (define e
    (entity 
      (direction '(0 1))
      (position  '(0 0)
                 (list (+ (first (get-position))
                          (first (get-direction)))
                       (+ (second (get-position))
                          (second (get-direction)))))))

  (define g0 (game e))

  (define g4 (ticks 4 g0))


  (define e4 (first (game-entities g4)))
  (define p4 (get-component e4 position?))


  (check-equal?
    (second (get-position p4)) 
    4))


