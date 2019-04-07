#lang racket

(require rackunit 
         "../main.rkt"
         "./util.rkt")

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



(test-case "Conway's game of life"
   (define-component conway (x y alive?))          

   (define (die c)  (set-conway-alive? c #f))
   (define (live c) (set-conway-alive? c #t))

   (define/contract (entity-xy=? x y)
      (-> number? number?
          (-> entity? boolean?))

      (lambda (e)
        (define my-x (entity-conway-x e))  
        (define my-y (entity-conway-y e))  

        (and 
          (= my-x x)
          (= my-y y))))

   (define/contract (neighbor dx dy)
      (-> number? number? (-> game? entity? (or/c entity? #f)))

      (lambda (g e)
       (define my-x (entity-conway-x e))
       (define my-y (entity-conway-y e))
       (get-entity* g
                    (entity-xy=? (+ my-x dx)
                                 (+ my-y dy)))))

   (define north      (neighbor  0 -1))
   (define south      (neighbor  0  1))
   (define east       (neighbor  1  0))
   (define west       (neighbor -1  0))
   (define north-west (neighbor -1 -1))
   (define north-east (neighbor  1 -1))
   (define south-west (neighbor -1  1))
   (define south-east (neighbor  1  1))

   (define (live-neighbors g e)
     (filter entity-conway-alive? 
             (filter identity
                     (list (north g e)
                           (south g e)
                           (west g e)
                           (east g e)
                           (north-east g e)
                           (north-west g e)
                           (south-east g e)
                           (south-west g e))))) 

   (define (conway-update g e c)
     (define n (length (live-neighbors g e)))
     (displayln n)
     (displayln c)

     (define ret (cond 
                   [(and (conway-alive? c)
                         (< n 2))
                    (die c)]
                   [(and (conway-alive? c)
                         (or (= n 2) (= n 3)))
                    (live c)]
                   [(and (conway-alive? c)
                         (> n 3))
                    (die c)]
                   [(and (not (conway-alive? c))
                         (= n 3))
                    (live c)]
                   [else c]))
       

     (displayln ret)

     ret)

   (define g0 (game 
               (entity (conway 0 0 #t #:update conway-update))
               (entity (conway 1 0 #t #:update conway-update))
               (entity (conway 2 0 #t #:update conway-update))

               (entity (conway 0 1 #t #:update conway-update))
               (entity (conway 1 1 #t #:update conway-update))
               (entity (conway 2 1 #t #:update conway-update))

               (entity (conway 0 2 #t #:update conway-update))
               (entity (conway 1 2 #t #:update conway-update))
               (entity (conway 2 2 #t #:update conway-update))))

   (define (conway-print g)
     ;TODO: Generalize to bigger grids

     (define es (game-entities g))  

     (define row1 (take es 3))
     (define row2 (take (drop es 3) 3))
     (define row3 (take (drop es 6) 3))

     (displayln (map entity-conway-alive? row1))
     (displayln (map entity-conway-alive? row2))
     (displayln (map entity-conway-alive? row3))) 

   (define gs (tick-list g0 3))

   (for ([g gs]
         [i (in-naturals)])
     (displayln i) 
     (conway-print g))

;TODO: Actual test 

;TODO: Stress test, speed benchmarks.

  )
