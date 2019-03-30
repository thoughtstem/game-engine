#lang racket

(require rackunit "../main.rkt")

;TODO: Can we do something with coroutine-like behaviour?

(module+ test ;Basic test for entity handlers
  (let ()

    (define e (entity (new-health 5 #:entity-handler entity:gain-health)))
    (define g (game e e e))

    (health-test g)))


(module+ test ;Basic test for component handlers 
  (let ()

    (define e (entity (new-health 5 #:handler gain-health)))
    (define g (game e e e))

    (health-test g)))

(module+ test ;Testing auto-generated component handler builder function
  (let ()

    (define e (entity (new-health 5 #:handler (update-health-amount add1))))
    (define g (game e e e))

    (health-test g)))

(module+ test ;Testing auto-generated entity handler builder function
  (let ()

    (define e (entity (new-health 5 #:entity-handler (update-entity-health-amount add1))))
    (define g (game e e e))

    (health-test g)))

(module+ test ;Testing auto-generated entity handler builder function
  (let ()

    (define e (entity (new-health 5 #:entity-handler (update-entity-health (update-health-amount add1)))))
    (define g (game e e e))

    (health-test g)))

(module+ test ;Testing auto-generated entity handler builder function
  (let ()

    (define e (entity (new-health 5 #:entity-handler (update-entity-health (new-health 6))))) ;This is different from most of the other tests.  After one tick, this entity will get a health component that stops updating, because it will get replaced with the version that has no handlers
    (define g (game e e e))

    (health-test g)))

(module+ test ;Testing auto-generated entity handler builder function
  (let ()

    (define e (entity (new-health 5 #:entity-handler (update-entity-first-health (update-health-amount add1)))))
    (define g (game e e e))

    (health-test g)))

(module+ test ;Testing adding a component
  (let ()

    (define no-health (entity))

    (define e (add-component no-health
                             (new-health 5 #:entity-handler (update-entity-health-amount add1))))

    (define g (game e e e))

    (health-test g)))

(module+ test ;Testing removing a component by predicate
  (let ()

    (define no-health (entity))

    (define e (add-component no-health
                             (new-health 5 #:entity-handler (update-entity-health-amount add1))))

    (define no-health-again (remove-component e health?))

    (check-equal? 0 (length (entity-components no-health-again)))))

(module+ test ;Testing removing a component by reference
  (let ()

    (define no-health (entity))

    (define h (new-health 5 #:entity-handler (update-entity-health-amount add1)))

    (define e (add-component no-health h))

    (define no-health-again (remove-component e h))

    (check-equal? 0 (length (entity-components no-health-again)))))




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
    (init-ids g))

  (check-pred (all-entities (curry has-id?)) started-g)

  (define ticked-g (tick started-g))

  (define (health=? amount e)
    (= amount (health-amount (get-component e health?))))

  (check-pred (all-entities (curry health=? 6)) ticked-g))



