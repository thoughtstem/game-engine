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

    (parameterize ([CURRENT-ENTITY e])
      (for ([c (entity-components e)]
            [ci (in-naturals)])
        (debug:component-tick-begin c)

        (define h (component-update c))

        (define next-e (list-ref (game-entities g) ei))

        (set-entity-changed?! next-e #f)


        ;WHyyyy don't we get better error line numbers from macro-defined functions?
        ;  Nope.  It's not the macros.  It's the way we catch errors in runtime.  Need to rethrow that shit somehow...  See "Error ticking entity" handler...
        (when h
          (define op
            (with-handlers

              ([exn:fail? (lambda (er)
                            (define e-string (pretty-print-entity-string e))
                            (error (~a "Error ticking entity\n" e-string "\n" (exn-message er))))])

              (if (mutable-state) ;This is confusing as shit.
                (let ([op (h g next-e c)])
                  (set! g 
                    (apply-op op g e c))
                  op) 
                (h g next-e c))))


          ;In the non-mutable case, the time before the op application we can still see the old and new versions of the entity.  I suppose we might want to do something in that intervening time, so we'll leave that space here.


          (when (not (mutable-state))
            (debug:applying-op op)
            (set! g (apply-op op g ei c)))

          (debug:after-entity-update g op c)


          ;TODO: should require that dead and spawner be at the top two slots of the component list -- faster querying that way...  And use the "dirty bit"
          (when (get-component op dead?)  
            (set! to-remove (cons e to-remove))
            (debug:added-to-remove-queue to-remove))

          (when (get-component op spawner?)
            (set! to-spawn (append (map spawner-to-spawn 
                                        (get-components op spawner?))
                                   to-spawn))
            (debug:added-to-spawn-queue to-spawn) 

            ;TODO: Get the verb tenses right here.
            (set! g (update-entity g op
                                   (curryr remove-component spawner?))) 

            (debug:stripped-spawner-from-entity op) 
            )


          ))))

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





