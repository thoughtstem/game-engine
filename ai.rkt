#lang racket

(module+ test
  (require rackunit
           "./components/storage.rkt")

  (define e (sprite->entity empty-image
                            #:name "player"
                            #:position (posn 0 0)))


  (let ()

    (define c-a (counter 5))
    (define a (make-state 'a c-a))

    (define c-b (counter 10))
    (define b (make-state 'b c-b))

    (check-equal? c-a
                  (first (state-components a)))

    (define e-with-a (add-components-from-state e a))

    (check-equal? (get-component e-with-a counter?)
                  c-a)

    (define e-with-updated-a ((change-counter-by 10) #f e-with-a))


    ;Check that removing components from state B does not remove the counter from state A
    (check component-eq?
           (get-component (remove-components-from-state e-with-updated-a b) counter?)
           c-a)
    

    (define e-without-a (remove-components-from-state e-with-updated-a a))

    (check-equal? (get-component e-without-a counter?)
                  #f)

    )

  (let ()
    
    (define not-boss (make-state 'not-boss
                                 (storage "aggression" 0)
                                 (storage "boss-mode" "No")))
    
    (define boss (make-state 'boss
                             (storage "aggression" 100)
                             (storage "boss-mode" "Yes")))

    (define not-boss->boss (make-transition #:rule (λ(g e)
                                                     (= 1 (get-counter e)))
                                            not-boss boss))
    
    (define e2 (add-component
                (entity-add-machine e
                                   (state-machine not-boss
                                                  (list not-boss boss)
                                                  (list not-boss->boss)))
                (counter 0)))

    (check-equal? (get-entity-current-state e2)
                  not-boss)
    
    (define game:pre-boss (tick #:ticks 20 ;Nothing should really happen
                                (initialize-game (list e2))))

    (define entity:pre-boss (get-entity "player" game:pre-boss))

    (check-equal? (get-entity-current-state entity:pre-boss)
                  not-boss)

    (check-equal? (get-storage-data "boss-mode" entity:pre-boss)
                  "No")

    (define game:right-before-boss (game-replace-entity game:pre-boss
                                                        (update-entity entity:pre-boss
                                                                       counter?
                                                                       (counter 1))))

    (define game:after-boss (tick #:ticks 1
                                  game:right-before-boss))

    (define entity:after-boss (get-entity "player" game:after-boss))

    (check-equal? (get-entity-current-state entity:after-boss)
                  boss)

    (check-equal? (get-storage-data "boss-mode" entity:after-boss)
                  "Yes")

    (check-equal? (get-storage-data "aggression" entity:after-boss)
                  100)

    )


  (let ()
    
    (define a (make-state 'a
                          (counter 0)
                          (every-tick (change-counter-by 1))))
    
    (define b (make-state 'b
                          (counter 5)
                          (every-tick (change-counter-by 1))))

    (define a->b (make-transition #:rule (λ(g e)
                                           (eq? 'b (get-storage-data "current-counter" e)))
                                  a b))

    (define b->a (make-transition #:rule (λ(g e)
                                           (eq? 'a (get-storage-data "current-counter" e)))
                                  b a))
    
    (define starting-e (add-component
                        (entity-add-machine e
                                            (state-machine a
                                                           (list a b)
                                                           (list a->b b->a)))
                        (storage "current-counter" 'a)))

    
    
    (define game:a=20 (tick #:ticks 20 
                            (initialize-game (list starting-e))))

    (define entity:a=20 (get-entity "player" game:a=20))

    (check-equal? (get-entity-current-state entity:a=20)
                  a)


    (check-equal? (get-entity-current-state entity:a=20) a)
    (check-equal? (get-counter entity:a=20) 20)


    (define game:b=5 (tick #:ticks 1
                           (game-replace-entity game:a=20
                                                (set-storage "current-counter" entity:a=20 'b))))


    (define entity:b=5 (get-entity "player" game:b=5))


    (check-equal? (get-entity-current-state entity:b=5) b)


    (check-equal? (get-counter entity:b=5) 6) ;Started at 5, we've done 1 tick since transitioning

    (define game:b=25 (tick #:ticks 19 game:b=5))

    (define entity:b=25 (get-entity "player" game:b=25))

    (check-equal? (get-counter entity:b=25)
                  25)))

(provide move-up-and-down
         move-left
         move-right
         move-up
         move-down
         move-random
         move-dir-spd
         move
         move-up-down
         move-left-right
         spin

         (rename-out [make-state state])
         (rename-out [make-transition transition])
         (rename-out [make-state-machine state-machine])
         entity-add-machine)

(require posn)
(require "./game-entities.rkt")
(require 2htdp/image threading)
(require "./components/animated-sprite.rkt")
(require "./components/direction.rkt")
(require "./components/speed.rkt")
(require "./components/every-tick.rkt")
(require "./components/counter.rkt")

(struct state (name components) #:transparent)
(struct transition (rule source target) #:transparent)

(struct state-machine (current states transitions) #:transparent)

(define/contract (make-state name . components)
  (->* (any/c) #:rest (listof component?) state?)
  (state name components))

(define (make-transition source target #:rule rule)
  (transition rule source target))

(define (make-state-machine start states transitions)
  (state-machine start states transitions))


(define (get-entity-current-state e)
  (state-machine-current (get-component e state-machine?)))

(define (is-out-going-from? current t)
  (eq? current
       (transition-source t)))

(define (should-trigger? g e t)
  ((transition-rule t) g e))

(define (transition-if-necessary g entity-with-machine machine)
  (match-define
    (state-machine current states transitions)
    machine)

  (define outgoing-edges (filter (curry is-out-going-from? current) transitions))

  (define triggered-edges (filter (curry should-trigger? g entity-with-machine) outgoing-edges))

 
  (if (empty? triggered-edges)
      machine
      (begin
        (state-machine (transition-target (first triggered-edges))
                       states
                       transitions)))  )



(define (remove-components-from-state e s)
  (define components (state-components s))

  (foldl
     (λ(n a)
       (remove-component a (curry component-eq? n))) 
     e
     components))

(define (add-components-from-state e s)
  (define components (state-components s))

  (add-components e components))

(define (entity-add-machine e m)
  (~> e
      (add-component _ m)
      (add-components-from-state _ (state-machine-current m))))

(define (entity-switch-machine e new-machine)
  (define old-state (get-entity-current-state e))
  (define new-state (state-machine-current new-machine))

  (~> e
      (update-entity _ state-machine? new-machine)
      (remove-components-from-state _ old-state)
      (add-components-from-state _ new-state)))

(define (update-state-machine g e c)
  (define new-machine        (transition-if-necessary g e c))
  
  (if (eq? c new-machine)
      e
      (entity-switch-machine e new-machine)))


(new-component state-machine?
               update-state-machine)



;A Lot of these could be implemented better (less stateful?)
;  Or go full state machine?
;  Waypoint system?

;Everything feels a bit cobbled together at the moment.

(define (move-up-and-down #:min min #:max max #:speed s)
  (define f (curry + s))
  (lambda (g e)
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (if (>= current-pos-y max)
        (set! f (curryr - s))
        (void))
    (if (<= current-pos-y min)
        (set! f (curry + s))
        (void))
    (update-entity e posn? (posn current-pos-x
                                 (f current-pos-y)))))

(define (move-left #:speed s)
  (lambda (g e)
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (update-entity e posn? (posn (- current-pos-x s)
                                 current-pos-y))))

(define (move-right #:speed s)
  (lambda (g e)
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (update-entity e posn? (posn (+ current-pos-x s)
                                 current-pos-y))))

(define (move-up #:speed s)
  (lambda (g e)
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (update-entity e posn? (posn current-pos-x
                                 (- current-pos-y s)))))

(define (move-down #:speed s)
  (lambda (g e)
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (update-entity e posn? (posn current-pos-x
                                 (+ current-pos-y s)))))

(define (move-random #:speed s)
  (define rx (* s (random -1 2)))
  (define ry (* s (random -1 2)))
  (lambda (g e)
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (update-entity e posn? (posn (+ rx current-pos-x)
                                 (+ ry current-pos-y)))))

(define (move-dir-spd #:dir d #:speed s)
  (lambda (g e)
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (define x-vel (* (cos (degrees->radians d)) s))
    (define y-vel (* (sin (degrees->radians d)) s))
    (update-entity e posn? (posn (+ current-pos-x x-vel)
                                 (+ current-pos-y y-vel)))))
(define (move)
  (lambda (g e)
    (define d (get-direction e))
    (define s (get-ai-speed e))
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (define x-vel (* (cos (degrees->radians d)) s))
    (define y-vel (* (sin (degrees->radians d)) s))
    (update-entity e posn? (posn (+ current-pos-x x-vel)
                                 (+ current-pos-y y-vel)))))

(define (spin #:speed s)
  (lambda (g e)
    (define f (λ(i) (rotate s i)))
    (update-entity e animated-sprite? (curry sprite-map f))))

(define (move-up-down #:min min #:max max)
  (lambda (g e)
    (define d (get-direction e))
    (define s (get-ai-speed e))
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (define x-vel (* (cos (degrees->radians d)) s))
    (define y-vel (* (sin (degrees->radians d)) s))
    (update-entity (cond [(>= current-pos-y max) (update-entity e direction? (direction 270))]
                         [(<= current-pos-y min) (update-entity e direction? (direction 90))]
                         [else e])
                   posn? (posn (+ current-pos-x x-vel)
                               (+ current-pos-y y-vel)))))


(define (move-left-right #:min min #:max max)
  (lambda (g e)
    (define d (get-direction e))
    (define s (get-ai-speed e))
    (define current-pos-y (posn-y (get-component e posn?)))
    (define current-pos-x (posn-x (get-component e posn?)))
    (define x-vel (* (cos (degrees->radians d)) s))
    (define y-vel (* (sin (degrees->radians d)) s))
    (update-entity (cond [(>= current-pos-x max) (update-entity e direction? (direction 180))]
                         [(<= current-pos-x min) (update-entity e direction? (direction 0))]
                         [else e])
                   posn? (posn (+ current-pos-x x-vel)
                               (+ current-pos-y y-vel)))))

