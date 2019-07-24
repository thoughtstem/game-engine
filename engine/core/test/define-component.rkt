#lang racket

(require rackunit "../main.rkt" "./util.rkt")

(define (check-game g)
  (check-all-entities-health g 5)
  (check-all-entities-health (tick g) 6))


(test-case "two components updating on the same entity"
  
           (define-component c1 number?)
           (define-component c2 number?)

           (define g
             (game
               (entity
                 (c1 0 (+ 1 (get-c1)))      
                 (c2 0 (+ 2 (get-c2))))))


           (define g4 (ticks 4 g))
           (define e4 (first (game-entities g4)))
           (define c1-4 (get-component e4 c1?))
           (define c2-4 (get-component e4 c2?))

           (check-equal?
             (get-c2 c2-4)
             8)

           (check-equal?
             (get-c1 c1-4)
             4))

(test-case "update:COMPONENT/FIELD handler, taking a field -> field function"
           (define e (entity (health 5 
                                     (+ 1 (get-health)))))

           (define g (game e e e))

           (check-game g))


(test-case "Doing an update based on a rule that looks at a different entity"
           (define-component counter (n))
           (define e (entity 
                       (counter 0)
                       (health 5 
                               (if (= 0 (get-counter))
                                 (add1 (get-health))
                                 (get-health)))))

           (check-game (game e e e)))

(test-case "Testing adding a dead component and entity removal"

           (define (poisoned)
             (sub1 (get-health)))

           (define-component die-on-0-health
                             (or/c #f despawn?))

           (define (e s)
             (entity 
               (health s (poisoned))
               (die-on-0-health #f 
                                (if (= 0 (get-health))
                                  (despawn) 
                                  #f))))

           (define g0 (game (e 3) (e 2) (e 1)))

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
             "All entities should be dead"))

