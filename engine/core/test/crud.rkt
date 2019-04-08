#lang racket

(require rackunit 
         "../main.rkt"
         "./util.rkt")

(test-case "Create/Read/Update/Destroy component"
  
           (define-component health (amount))

           (define no-health (entity))
           (define h (health 5))

           (define e (add-component no-health h))

           (check-not-false
             (get-component e health?))

           (define e-with-more-health (update-component e health? 
                                                        (curryr update:health/amount add1)))

           (check-equal?
             (read:health/amount e-with-more-health)
             6)

           (define no-health-again (remove-component e h))
           
           (check-false
             (get-component no-health-again health?)))
