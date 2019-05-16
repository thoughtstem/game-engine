#lang racket

(provide hero-movement)

(require "../../main.rkt")

(define (hero-movement)
  (movement-system #:direction-update (thunk* (as-posn (get-current-input)))))
