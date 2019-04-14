#lang racket

(require rackunit "../main.rkt" "./util.rkt")

(define (check-game g)
  (check-all-entities-health g 5)
  (check-all-entities-health (tick g) 6))


(test-case "update:COMPONENT/FIELD handler, taking a field -> field function"
           (define e (entity (health 5 #:update (update:health/amount^ add1))))
           (check-game (game e e e)))

(test-case "update:COMPONENT handler, taking a component -> component function"
           (define e (entity (health 5 #:update (update:health^ 
                                                  (curryr update:health/amount add1)))))
           (check-game (game e e e)))

;Hmmm... Do we want an auto generated convenience function for this?
;  Can we get update:health/amount^ to be attachable to other components?
(test-case "Updating a component from a different component on the same entity"
           ;It's really just the same because update:health/amount^
           (define e (entity (health 5)
                             (new-component #:update 
                                            (lambda (g e c)
                                              (update-component e 
                                                                health? 
                                                                (curryr update:health/amount add1))))))


           (check-game (game e e e)) )

(test-case "Doing an update based on a rule that looks at a different entity"
           (define-component counter (n))
           (define e (entity 
                       (counter 0)
                       (health 5 #:update (on-rule (rule:counter/n^ (curry = 0))
                                                   (update:health/amount^ add1)))))

           (check-game (game e e e)))

(let () ;test-case "Testing adding a dead component and entity removal"

           (define poisoned
             (update:health/amount^ sub1))

           (define die-on-0-health
             (on-rule (rule:health/amount^ (curry = 0))
                      (add-component^ (dead))))

           (define e1 
             (entity 
               (health 1 #:update 
                       (compose-handlers poisoned
                                         die-on-0-health))))


           (define e2
             (entity
               (health 2 #:update 
                       (compose-handlers poisoned
                                         die-on-0-health))))

           (define e3
             (entity
               (health 3 #:update 
                       (compose-handlers poisoned
                                         die-on-0-health))))  

           (define g0 (game e1 e2 e3))  
           (define g1 (tick g0)) 
           (define g2 (tick g1)) 
           (define g3 (tick g2)) 

           (check-equal?
             (length (game-entities g0))
             3
             "No entities should be dead")

           (check-equal?
             (length (game-entities g1))
             2
             "One entity should be dead")

           (check-equal?
             (length (game-entities g2))
             1
             "Two entities should be dead")

           (check-equal?
             (length (game-entities g3))
             0
             "All entities should be dead")
           

           )


(test-case "Testing removing a component from a handler"

           (define-component test ())

           (define e (entity 
                       (test)
                       (health 5 #:update (compose-handlers 
                                            (update:health/amount^ add1)
                                            (on-rule 
                                              (rule:health/amount^ (curry = 6))
                                              (remove-component^ test?))))))

           (define g0 (game e e e))
           (define g1  (tick g0))
           (define g2  (tick g1))

           (check-false
             (get-entity g2
                         (curryr has-component test?))
             "There should be no entities in the game with the (test) component")
           
           (check-equal?
             (read:health/amount (first (game-entities g2)))
             7))

