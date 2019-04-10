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


;CRUD WITH DELAYED OPS

(test-case "Add/Remove component to entity"
   (define e (entity))         

   (define e2 (apply-op-entity e 
                               (add-c (health 5))))
   (define e3 (apply-op-entity e2
                               (remove-c health?)))

   (check-equal?
     (length (entity-components e))
     0)

   (check-equal?
     (length (entity-components e2))
     1)
   
   (check-equal?
     (length (entity-components e3))
     0) 
   )




(test-case "Add/Remove entity from game"
   (define e (entity (health 5)))         

   (define g (game))

   (define g2 (apply-op-game g
                             (add-e e)))
   (define g3 (apply-op-game g2
                             (remove-e (curryr has-component health?))))

   (check-equal?
     (length (game-entities g))
     0)

   (check-equal?
     (length (game-entities g2))
     1)
   
   (check-equal?
     (length (game-entities g3))
     0) 

   )


