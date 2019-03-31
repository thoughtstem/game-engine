#lang racket

(require rackunit 
         "../main.rkt"
         "./health-setup.rkt")

(test-case "Basic test for entity handlers" 
           (define e (entity (new-health 5 #:entity-handler entity:gain-health)))
           (define g (game e e e))

           (health-test g))


(test-case "Basic test for component handlers" 
           (define e (entity (new-health 5 #:handler gain-health)))
           (define g (game e e e))

           (health-test g) )

(test-case "Testing auto-generated component handler builder function "
           (define e (entity (new-health 5 #:handler (update-health-amount add1))))
           (define g (game e e e))

           (health-test g) )

(test-case "Testing auto-generated entity handler builder function "

           (define e (entity (new-health 5 #:entity-handler (update-entity-health-amount add1))))
           (define g (game e e e))

           (health-test g) )

(test-case "Testing auto-generated entity handler builder function"

           (define e (entity (new-health 5 #:entity-handler (update-entity-health (update-health-amount add1)))))
           (define g (game e e e))

           (health-test g) )

(test-case "Testing auto-generated entity handler builder function "

           (define e (entity (new-health 5 #:entity-handler (update-entity-health (new-health 6))))) ;This is different from most of the other tests.  After one tick, this entity will get a health component that stops updating, because it will get replaced with the version that has no handlers
           (define g (game e e e))

           (health-test g))

(test-case "Testing auto-generated entity handler builder function "

           (define e (entity (new-health 5 #:entity-handler (update-entity-first-health (update-health-amount add1)))))
           (define g (game e e e))

           (health-test g) )

(test-case "Testing adding a component "

           (define no-health (entity))

           (define e (add-component no-health
                                    (new-health 5 #:entity-handler (update-entity-health-amount add1))))

           (define g (game e e e))

           (health-test g) )

(test-case "Testing removing a component by predicate "

           (define no-health (entity))

           (define e (add-component no-health
                                    (new-health 5 #:entity-handler (update-entity-health-amount add1))))

           (define no-health-again (remove-component e health?))

           (check-equal? 0 (length (entity-components no-health-again))) )

(test-case "Testing removing a component by reference "

           (define no-health (entity))

           (define h (new-health 5 #:entity-handler (update-entity-health-amount add1)))

           (define e (add-component no-health h))

           (define no-health-again (remove-component e h))

           (check-equal? 0 (length (entity-components no-health-again))) )  

