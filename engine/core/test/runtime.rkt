#lang racket

(require rackunit 
         "../main.rkt"
         "../spawns.rkt"
         "./health-setup.rkt")

(define (uniq l)
  (remove-duplicates (filter identity l)))

;Testing a runtime property of id uniqueness.
;
;In other words, when we pass in e three times to start-game, each will start to change over time, as ticks begin happening.  We need a way to distinguish between two versions of the same entity (i.e. one from a previous tick), even though they have changed.  And we also want to differentiate between the copies of e.  They all came from the same constructor, so we can't use eq?  Comparing by id is the answer, which is what entity=? does.  Since id uniqueness is so fundamental to entity=? comparisons, we should test to make sure the property never breaks.

;Here's how we can ensure that property on a game.
;  (actually a slightly stronger property -- that no two entities or components share an id on any given tick.  And they maintain their id from creation through updates.
(define (ensure-uniqueness! g)
  (define eis (map entity-id (game-entities g)))
  (define cis (map component-id (flatten (map entity-components (game-entities g)))))

  (define all-ids (append eis cis))

  (check-equal? (length all-ids)
                (length (remove-duplicates all-ids))
                "There were ids shared between entities and/or components"))


(test-case "Id uniqueness after initialization" 
           (define e (entity (new-component #:entity-handler entity:gain-health)))
           (define g (initialize-game (game e e e)))

           (pretty-print-game g)

           (ensure-uniqueness! g))

(test-case "Id uniqueness after new entity spawn" 
           ;Simple way to make a virus
           ;If e has a component with this game handler on it, then it will begin executing and spawn yet another clone, and so on
           (define (me) 
             (entity (new-component #:game-handler (spawn me))))

           (define s (spawner))

           (define g0 (game s (me)))

           (define g1 (initialize-game g0))
           (define g2 (tick g1))
           (define g3 (tick g2))


           ;All ids uniq?
           (ensure-uniqueness! g1)
           (ensure-uniqueness! g2) 
           (ensure-uniqueness! g3)
           
           ;Did spawning happen on each tick?
           (check-equal? (length (game-entities g1))
                         2)
           (check-equal? (length (game-entities g2))
                         3)
           (check-equal? (length (game-entities g3))
                         4))



