#lang racket

(require rackunit 
         "../main.rkt"
         "./util.rkt")

(define (check-game g0)
  (define g (init g0))
  (check-all-entities-health g 5)
  (check-all-entities-health (tick g) 6))

(let () ;test-case "Basic test for entity handlers"
           (define e (entity (new-health 5 #:update entity:gain-health)))
           (check-game (game e e e)))


(test-case "Basic test for component handlers" 
           (define e (entity (new-health 5 #:update gain-health)))
           (check-game (game e e e)))

(test-case "Testing auto-generated component handler builder function "
           (define e (entity (new-health 5 #:update (update-health-amount add1))))
           (check-game (game e e e)))

(test-case "Testing auto-generated entity handler builder function "

           (define e (entity (new-health 5 #:update (update-entity-health-amount add1))))
           (check-game (game e e e)) )

(test-case "Testing auto-generated entity handler builder function"

           (define e (entity (new-health 5 #:update (update-entity-health (update-health-amount add1)))))
           (check-game (game e e e)))

(test-case "Testing auto-generated entity handler builder function "

           (define e (entity (new-health 5 #:update (update-entity-health (new-health 6))))) ;This is different from most of the other tests.  After one tick, this entity will get a health component that stops updating, because it will get replaced with the version that has no handlers
           (check-game (game e e e)) )

(test-case "Testing auto-generated entity handler builder function "

           (define e (entity (new-health 5 #:update (update-entity-first-health (update-health-amount add1)))))
           (check-game (game e e e)) )

(begin ;test-case "Testing adding a component "


           (define no-health (entity))

           (define e (add-component no-health
                                    (new-health 5 #:update (update-entity-health-amount add1))))

           (check-game (game e e e)))

(test-case "Testing removing a component by predicate "

           (define no-health (entity))

           (define e (add-component no-health
                                    (new-health 5 #:update (update-entity-health-amount add1))))

           (define no-health-again (remove-component e health?))

           (check-equal? 0 (length (entity-components no-health-again))) )

(test-case "Testing removing a component by reference "

           (define no-health (entity))

           (define h (new-health 5 #:update (update-entity-health-amount add1)))

           (define e (add-component no-health h))

           (define no-health-again (remove-component e h))

           (check-equal? 0 (length (entity-components no-health-again))) )  

