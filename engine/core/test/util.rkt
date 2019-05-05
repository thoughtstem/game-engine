#lang racket

(require rackunit "../main.rkt")

;This is a util file that creates a basic component -- a fictitiously simple "health" component with some functions on it.   We use this user story across a variety of our tests.  

;It also provides other useful things to make tests more lean and to-the-point

(provide health
         set-health
         get-health
         health?

         ensure-uniqueness!

         check-all-entities-health)

(define-component health number?)


(define (check-all-entities-health g amount)
  (define (health=? amount e)
    (= amount (get-health (get-component e health?))))

  (check-pred (all-entities (curry health=? amount)) 
              g
              (~a "Not all entities had " amount " health. "))
  
  (void)) 

(define (ensure-uniqueness! g)
  (define eis (map entity-id (game-entities g)))
  (define cis (map component-id (flatten (map entity-components (game-entities g)))))

  (define all-ids (append eis cis))

  (check-equal? (length all-ids)
                (length (remove-duplicates all-ids))
                "There were ids shared between entities and/or components"))
