#lang racket 

(require rackunit
         "../main.rkt"
         "./util.rkt")

#;
(test-case "for-ticks"
           (define e (entity 
                       (new-health 5 
                                   #:update 
                                   (for-ticks 5 
                                              ;TODO: Gross, can we not have to lift the function?
                                              (component-handler->game-handler gain-health)))))
         
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

(test-case "times + for-ticks"
           (define e (entity 
                       (new-health 5 
                                   #:update 
                                   (times 2
                                          ;TODO: Gross, can we not have to lift the function?
                                          (for-ticks 2
                                                     gain-health)))))

           (define g0 (game e))
           (define e0 (first (game-entities g0)))

           (define gs (tick-list g0 10))

           (check-equal? 
             (entity-health-amount e0)
             5)

           (for ([g (drop gs 4)])
             (define e (first (game-entities g)))
             (check-equal?
               ;Gross, can we have an easier way to pluck out the entity + health + amount?
               (entity-health-amount e)
               9)))



