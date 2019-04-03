#lang racket

(require rackunit
         "../main.rkt"
         game-engine/engine/core
         game-engine/engine/core/test/util)

(test-case "Id uniqueness after new entity spawn" 

           ;Simple way to make a virus
           ;If e has a component with this game handler on it, then it will begin executing and spawn yet another clone, and so on
           (define (me) 
             (entity (new-component #:update (spawn me))))

           (define s (spawn-manager))

           (define g0 (game s (me)))

           (define g1 (init g0))
           (define g2 (tick g1))
           (define g3 (tick g2))


           ;All ids uniq?
           (ensure-uniqueness! g1)
           (ensure-uniqueness! g2) 
           (ensure-uniqueness! g3)

           ;We start with the given two entities
           (check-equal? (length (game-entities g1))
                         2)
           ;The first spawn is triggered, but the spawned entities take a pitstop in the spawner first
           (check-equal? (length (game-entities g2))
                         2)
           ;When the spawner gets a chance to update, it transfers its entities into the game
           (check-equal? (length (game-entities g3))
                         3))

(test-case "Spawn queue populated correctly" 

           (define bullet (entity))
           (define ship   (entity (new-component #:update (spawn bullet))))

           (define s (spawn-manager))

           (define g0 (game s ship))

           (define g1 (init g0))

           (define gs (tick-list g1 10))

           ;Not empty at first
           (check-true
             (spawn-queue-empty? (first gs))
             "The spawn queue starts empty.")

           ;Empty from then on
           (for ([g (rest gs)])
             (check-false
               (spawn-queue-empty? g)  
               "The spawn queue is not empty when the ship starts spawning.."))


           ;TODO: Just to keep testing things, we may need a way to better control game handlers -- e.g. run once... or coroutine-like, run/yield, etc...

           )

(test-case "Spawn queue stops spawning if no calls to spawn happen" 

           ;Quick handler orgy here...  TODO: Doc and test.

           (define bullet (entity))
           (define ship   (entity (new-component 
                                    #:update 
                                    (for-ticks 5
                                              (spawn bullet)))))

           (define s (spawn-manager))

           (define g0 (game s ship))
           (define g1 (init g0))
           (define gs (tick-list g1 10))

           ;(map pretty-print-game gs)

           (define qs (map spawn-queue-empty? gs))

           (check-true
             (spawn-queue-empty? (first gs))
             "The spawn queue starts empty.")

           (for ([g (take (rest gs) 5)])
             (check-false
               (spawn-queue-empty? g)  
               "The spawn queue is not empty when the ship starts spawning."))

           (for ([g (drop (rest gs) 5)])
             (check-true
               (spawn-queue-empty? g)  
               "The spawn queue is empty again"))


           ;TODO: Just to keep testing things, we may need a way to better control game handlers -- e.g. run once... or coroutine-like, run/yield, etc...

          ;   Brainstorm, what are our "control structures"?
          ;     for-ticks, every-other-tick, switch-between 
          ;     pick-randomly
          ;     state-machine???
          ;     
          ;     while-property-true
          ;     until
          ;     on-rule
          ; 
          ;   And what are the primitives for creating these?

           )


;TODO: Tests where there are multiple things contributing to the spawn queue.
;TODO: Test dying or despawning -- whether it uses the spawn-manager or not.






