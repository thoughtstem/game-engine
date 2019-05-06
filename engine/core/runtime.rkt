#lang racket

(provide init 
         tick
         tick!
         debug-tick
         debug
         ticks
         tick-component

         tick-list

         all-entities
         has-id?
         )

(require "./base.rkt"
         "./crud.rkt"
         "./util.rkt"
         "./handler-util.rkt"
         "./spawner.rkt"
         "./printer.rkt"
         "./debug.rkt")



(define to-remove '())
(define to-spawn  '())

(define (tick! g)
  (mutable! (tick g)))

(define/contract (tick g)
  (maybe-contract
    (-> game? game?))

  (debug:tick-begin g)
  
  (define next-g #f)
 
  (if (mutable-state)
    (set! next-g g)
    (set! next-g (struct-copy game g)))

  (set! next-g (tick-entities next-g)) 
  (set! next-g (handle-removals next-g))
  (set! next-g (handle-spawns next-g))

  next-g)


(define (tick-entities g)
  (for ([e (game-entities g)]
        [ei (in-naturals)])
    (debug:entity-tick-begin e)

    (define next-e
      (if (mutable-state)
        e
        (struct-copy entity e) ))

    ;Tick the components
    (parameterize ([CURRENT-ENTITY next-e]
                   [CURRENT-GAME g])
      (for ([c (entity-components e)]
            [ci (in-naturals)])
        (debug:component-tick-begin c)

        (define h (component-update c))

        (when h
          
          (set! c (h c))

          (hash-set! (entity-lookup next-e)
                     (vector-ref c 1) ;Gross...  Gotta hide all the explicit vector nonsense
                     c)

          (set-entity-components! next-e
                                  (list-set
                                    (entity-components next-e)
                                    ci
                                    c)) 

          (define v (get-value c)) 

          (when (despawn? v)
            (set! to-remove (cons next-e to-remove))
            (debug:added-to-remove-queue to-remove))

          (when (spawn? v)
            (set! to-spawn (cons (spawn-entity v) to-spawn))
            (debug:added-to-spawn-queue to-spawn) ))

        (debug:component-tick-end c)))
    


    (set-game-entities! g
                        (list-set (game-entities g) ei next-e))) 


  (debug:all-entities-ticked g)
  g)

(define (handle-removals g)

  (for ([r to-remove])
    (set! g (remove-entity g r)))

  (when (not (empty? to-remove))
    (debug:after-removals g to-remove))

  (set! to-remove '())
  g)

(define (handle-spawns g)

  (for ([s to-spawn])
    (set! g (add-entity g 
                        (copy-entity s))))
  
  (when (not (empty? to-spawn))
    (debug:after-spawns g to-spawn))

  (set! to-spawn '())
  g)




(define/contract (ticks n g)
                 (-> number? game? game?)
                 (if (= 0 n) 
                   g 
                   (ticks (sub1 n) 
                          (tick g))))

(define/contract (tick-list g n)
                 (-> game? positive? (listof game?))

                 (if (= n 1)
                   (list g) 
                   (cons g
                         (tick-list (tick g) 
                                    (sub1 n)))))

(define (tick-component g e c)
  (define h (component-update c))
  (h g e c))

;Does this get used?  Is it old?
(define/contract (has-id? e)
  (maybe-contract
    (-> entity? boolean?))
                 (number? (entity-id e)))

(define (all-entities pred?)
  (lambda (g)
    (andmap pred? (game-entities g))))

(define (apply-op o g ei c)
  (cond
    [(entity? o) (begin 
                   (update-entity g ei o))]
    [else (raise (~a "Unsupported handler return value: " o))]) )

(define (debug-tick g)
  (debug
    (tick g)))





