#lang racket

(require rackunit "../main.rkt")

;This is a util file that creates a basic component -- a fictitiously simple "health" component with some functions on it.   We use this user story across a variety of our tests.  

;It also provides other useful things to make tests more lean and to-the-point

(provide health-amount 
         health?
         health

         entity-health-amount?
         entity-health-amount
         update-health-amount
         update-entity-health-amount
         update-entity-health
         ensure-uniqueness!
         
         entity:gain-health
         gain-health
         check-all-entities-health)

(define-component health (amount))

(define/contract (gain-health h)
                 (-> health? health?)

                 (set-health-amount h
                                    (add1 (health-amount h))))

(define/contract (entity:gain-health e h)
 (-> entity? health? entity?)

 (update-component e health? ;Could have done h instead of health?, but why not test predicate-based update? 
                     gain-health))

(define (check-all-entities-health g amount)
  (define (health=? amount e)
    (= amount (health-amount (get-component e health?))))

  (check-pred (all-entities (curry health=? amount)) 
              g
              (~a "Not all entities had " amount " health. "))
  
  (void)) 

;Testing a runtime property of id uniqueness.
;
;In other words, when we pass in e three times to start-game, each will start to change over time, as ticks begin happening.  We need a way to distinguish between two versions of the same entity (i.e. one from a previous tick), even though they have changed.  And we also want to differentiate between the copies of e.  They all came from the same constructor, so we can't use eq?  Comparing by id is the answer, which is what entity=? does.  Since id uniqueness is so fundamental to entity=? comparisons, we should test to make sure the property never breaks.

;Here's how we can ensure that property on a game.
;  (actually a slightly stronger property -- that no two entities or components share an id on any given tick.  And they maintain their id from creation through updates.)
(define (ensure-uniqueness! g)
  (define eis (map entity-id (game-entities g)))
  (define cis (map component-id (flatten (map entity-components (game-entities g)))))

  (define all-ids (append eis cis))

  (check-equal? (length all-ids)
                (length (remove-duplicates all-ids))
                "There were ids shared between entities and/or components"))
