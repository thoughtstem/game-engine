#lang racket

(require rackunit "../main.rkt")

(define-component health (amount))

;TODO: Can we get better error messages??
;  If it dies in a function we defined, I'd like to get errors for those line numbers.  Maybe even willing to use a special define-component-handler function...

(define (gain-health h)
  (health (add1 (health-amount h))))

(define e (entity (health 5 #:handler gain-health)))

(define g (game e e e))

(define started-g 
  (init-ids g))

(check-pred (all-entities (curry has-id?)) started-g)

(define ticked-g (tick started-g))

(define (health=? amount e)
  (= amount (health-amount (get-component e health?))))

(check-pred (all-entities (curry health=? 6)) ticked-g)



