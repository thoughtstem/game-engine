#lang racket

(require rackunit "../main.rkt")

;This is a util file that creates a basic component -- a fictitiously simple "health" component with some functions on it.   We use this user story across a variety of our tests.  

(provide (struct-out health) ;TODO: component-out?
         new-health
         update-health-amount
         update-entity-health-amount
         update-entity-health
         update-entity-first-health
         
         entity:gain-health
         gain-health
         health-test)

(define-component health (amount))

(define/contract (gain-health h)
                 (-> health? health?)

                 (struct-copy health h 
                              [amount (add1 (health-amount h))]))

(define/contract (entity:gain-health e h)
                 (-> entity? health? entity?)
                 (update-component e health? ;Could have done h instead of health?, but why not test predicate-based update? 
                                   gain-health))

(define (health-test g)
  (define started-g 
    (initialize-game g))

  (check-pred (all-entities (curry has-id?)) started-g)

  (define ticked-g (tick started-g))

  (define (health=? amount e)
    (= amount (health-amount (get-component e health?))))

  (check-pred (all-entities (curry health=? 6)) ticked-g)) 

