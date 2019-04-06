#lang racket

(require rackunit 
         "../main.rkt"
         "./util.rkt")

(define-component dead ())
(define is-zero? (curry = 0))

(define e
  (entity 
    (health 3 #:update (compose-handlers
                         (update-health-amount sub1)
                         (on-rule 
                           (entity-health-amount? is-zero?) 
                           (compose-handlers
                             (log "here")
                             (add-component* (dead))
                             (remove-self)))))))

(define g (game e))

;Make a ticks function...  OR tick could take a kw
(tick (tick (tick (tick g))))

(displayln "Yay, we got here.... now write some real tests.")
