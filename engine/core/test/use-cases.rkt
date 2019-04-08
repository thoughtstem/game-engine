#lang racket

(require rackunit 
         "../main.rkt"
         "./util.rkt")

(require threading)

(test-case "Running out of health and dying"
  (define-component dead ())
  (define is-zero? (curry = 0))

  (define e
    (entity 
      (health 3 #:update (compose-handlers
                           (update-health-amount sub1)
                           (on-rule 
                             (entity-health-amount? is-zero?) 
                             (compose-handlers
                               (add-component (dead))
                               (remove-self)))))))

  (define g0 (game e))

  (define g4 (ticks 4 g0))
  (define e4 (first (game-entities g4)))

  (check-not-false
    (has-component* e4 dead?)
    "Entity should have the dead component")

  (check-false
    (has-component* e4 health?)
    "Entity should not have the health component"))



(test-case "Moving x and y"
  ;TODO: Put in docs.
  ;TODO: Maybe this shouldn't be in core? 
  ;  Movement module?

  (define-component position  (x y))
  (define-component direction (x y))
  (define-component speed (level))
  (define-component movement ())

  (define/contract (update-position-from-direction dir pos)
      (-> component? component? component?)

      (define dir-x (direction-x dir)) 
      (define dir-y (direction-y dir)) 
      (define pos-x (position-x pos)) 
      (define pos-y (position-y pos)) 

      (position (+ pos-x dir-x)
                (+ pos-y dir-y))) 

  (define/contract (entity-update-position-from-direction)
      (-> entity-handler?)
      (lambda (g e c)
        (define dir (entity-direction e))
        (define pos (entity-position e))
        (define new-pos (update-position-from-direction dir pos))
        (update-component* e position? new-pos)))

  (define e
    (entity 
      (position  0 0)
      (direction 0 1)
      (movement #:update (entity-update-position-from-direction))))

  (define g0 (game e))

  (define g4 (ticks 4 g0))
  (define e4 (first (game-entities g4)))
  (define p4 (get-component* e4 position?))

  (check-equal?
    (position-y p4) 
    4))




;TODO: Actual test 

;TODO: Stress test, speed benchmarks.





