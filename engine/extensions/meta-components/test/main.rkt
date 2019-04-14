#lang racket

(require rackunit 
         "../../../core/main.rkt"
         "../for-ticks.rkt"
         "../sequence.rkt"
         "../times.rkt"
         )


(test-case "for-ticks component"
           ;Test
           (define-component health (amount))
           (define poisoned (new-component #:update 
                                           (update:health/amount^ sub1)))

           (define e (entity (health 5)
                             (for-ticks 2 poisoned)))

           ;TODO: Macroify this...
           (define g0 (game e))
           (define g1 (tick g0))
           (define g2 (tick g1))
           (define g3 (tick g2))

           ;AND THIS
           (define e1 (get-entity g1 e))
           (define e2 (get-entity g2 e))
           (define e3 (get-entity g3 e))

           (check-equal?
             (health-amount (get-component e1 health?)) 
             4
             "An entity that starts with 5 health and the poisoned-for-two-ticks condition should have 4 health after the first tick")

           (check-equal?
             (health-amount (get-component e2 health?)) 
             3
             "An entity that starts with 5 health and the poisoned-for-two-ticks condition should have 3 health after the second tick")

           (check-equal?
             (health-amount (get-component e3 health?)) 
             3
             "An entity that starts with 5 health and the poisoned-for-two-ticks condition should STILL have 3 health after the third tick")
           )

(test-case "for-ticks + spawn"
           (define fireball (entity))
           (define e (entity (for-ticks 2 
                                        (spawn fireball))))


           (define gs (tick-list (game e) 4))

           (check-equal?
             (map (lambda (g) (length (game-entities g))) 
                  gs) 
             '(1 2 3 3)))

(test-case "for-ticks component x2"
           ;Test
           (define-component health (amount))
           (define-component mana (amount))

           (define poisoned (new-component #:update 
                                           (update:health/amount^ sub1)))

           (define enchanted (new-component #:update 
                                            (update:mana/amount^ add1)))

           (define e (entity (health 5)
                             (mana 5) 
                             (for-ticks 2 poisoned)
                             (for-ticks 2 enchanted)))

           (define g0 (game e))
           (define g1 (tick g0))
           (define g2 (tick g1))
           (define g3 (tick g2))

           (define e1 (get-entity g1 e))
           (define e2 (get-entity g2 e))
           (define e3 (get-entity g3 e))

           (check-equal?
             (mana-amount (get-component e1 mana?)) 
             6
             "An entity that starts with 5 mana and the enchanted-for-two-ticks condition should have 6 mana after the first tick")

           (check-equal?
             (mana-amount (get-component e2 mana?)) 
             7
             "An entity that starts with 7 mana and the enchanted-for-two-ticks condition should have 3 mana after the second tick")

           (check-equal?
             (mana-amount (get-component e3 mana?)) 
             7
             "An entity that starts with 7 mana and the enchanted-for-two-ticks condition should STILL have 3 mana after the third tick")
           )


(test-case "sequence component"
           ;Test
           (define-component health (amount))
           (define-component mana (amount))

           (define poisoned (new-component #:update 
                                           (update:health/amount^ sub1)))

           (define enchanted (new-component #:update 
                                            (update:mana/amount^ add1)))

           (define e (entity (health 5)
                             (mana 5) 
                             (sequence
                               (for-ticks 2 poisoned)
                               (for-ticks 2 enchanted))))

           (define g0 (game e))
           (define g1 (tick g0))
           (define g2 (tick g1))
           (define g3 (tick g2))
           (define g4 (tick g3))
           (define g5 (tick g4))

           (define e1 (get-entity g1 e))
           (define e2 (get-entity g2 e))
           (define e3 (get-entity g3 e))
           (define e4 (get-entity g4 e))
           (define e5 (get-entity g5 e))


           (check-equal?
             (health-amount (get-component e1 health?)) 
             4
             "An entity with a sequence of being poisoned for two ticks and enchanted for two ticks should lose one health on the first tick")

           (check-equal?
             (mana-amount (get-component e1 mana?)) 
             5
             "An entity with a sequence of being poisoned for two ticks and enchanted for two ticks should lose 0 mana on the first tick")

           (check-equal?
             (health-amount (get-component e2 health?)) 
             3
             "An entity with a sequence of being poisoned for two ticks and enchanted for two ticks should lose 2 health by 2nd tick")

           (check-equal?
             (mana-amount (get-component e2 mana?)) 
             5
             "An entity with a sequence of being poisoned for two ticks and enchanted for two ticks should have lost 0 mana by the 2nd tick")

           (check-equal?
             (health-amount (get-component e3 health?)) 
             3
             "An entity with a sequence of being poisoned for two ticks and enchanted for two ticks should stop losing health after the 2nd tick ")

           (check-equal?
             (mana-amount (get-component e3 mana?)) 
             6
             "An entity with a sequence of being poisoned for two ticks and enchanted for two ticks should gain 1 mana on the 3rd tick")

           (check-equal?
             (health-amount (get-component e4 health?)) 
             3
             "An entity with a sequence of being poisoned for two ticks and enchanted for two ticks should stop losing health after the 2nd tick")

           (check-equal?
             (mana-amount (get-component e4 mana?)) 
             7
             "An entity with a sequence of being poisoned for two ticks and enchanted for two ticks should have gained 2 mana by the 4th tick")

           (check-false
             (get-component e5 sequence?) 
             "A sequence that lasts 4 ticks should remove itself on the 5th tick")
           
           )


(test-case "sequence+times"
           (define-component health (amount))
           (define-component mana (amount))

           (define poisoned (new-component #:update 
                                           (update:health/amount^ sub1)))

           (define enchanted (new-component #:update 
                                            (update:mana/amount^ add1)))

           (define e (entity (health 5)
                             (mana 5) 
                             (times 2
                                    (sequence
                                      (for-ticks 2 poisoned)
                                      (for-ticks 2 enchanted)))))

           (define gs (tick-list (game e) 9))
           
           (check-equal?
             ;The health sequence
             (map (curryr get-field first health? health-amount) gs) 
             '(5     ;Starts at 5
               4 3   ;Poisoned for 2 ticks
               3 3   ;Not poisoned.  Enchanted 
               2 1   ;Poisoned again. 
               1 1   ;Enchanted again
               ))

           (check-equal?
             ;The mana sequence
             (map (curryr get-field first mana? mana-amount) gs) 
             '(5     ;Starst at 5
               5 5   ;Not enchanted
               6 7   ;Enchanted for 2 ticks 
               7 7   ;Not enchanted 
               8 9   ;Enchanted again
               )))







