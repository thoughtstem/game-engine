#lang racket 

(require rackunit
         "../main.rkt"
         "./util.rkt")

(test-case "for-ticks"
           (define e (entity 
                       (health 5 
                               #:update 
                               (for-ticks 5 (update:health/amount^ add1)))))

           (define g0 (game e))
           (define e0 (first (game-entities g0)))

           (define gs (tick-list g0 10))

           ;Starts at 5
           (check-equal? 
             (entity-health-amount e0)
             5)

           ;6,7,8,9,10

           ;And it's 10 from now on.  for-ticks ensured that the handler only ran 5 times
           (for ([g (drop gs 5)])
             (define e (first (game-entities g)))
             (check-equal?
               ;Gross, can we have an easier way to pluck out the entity + health + amount?
               (entity-health-amount e)
               10)))

