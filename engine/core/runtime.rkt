#lang racket

(provide init 
         tick
         ticks
         tick-component

         tick-list

         all-entities
         has-id?
         debug-mode
         )

(require "./base.rkt"
         "./crud.rkt"
         "./util.rkt"
         "./handler-util.rkt"
         "./spawner.rkt"
         "./printer.rkt"
         )

(define debug-mode (make-parameter #f))

(define next-g #f)

(define to-remove '())
(define to-spawn  '())

(define #;/contract 
  (tick g)
  #;
  (-> game? game?)

  #;
  (debug-hook:tick-begin g)

  #;
  (set! next-g (struct-copy game g))

  (set! next-g g)

  (tick-entities next-g) 
  (handle-removals next-g)
  (handle-spawns next-g)

  next-g)


(define (tick-entities g)
  (for ([e (game-entities g)]
        [ei (in-naturals)])
    (for ([c (entity-components e)]
          [ci (in-naturals)])

      (define h (component-update c))

      (define next-e (list-ref (game-entities next-g) ei))

      (set-entity-changed?! next-e #f)


      (when h

        ;This gives a slowdown of about 8 FPS (per 1000 E)
        (define op
          (with-handlers

            ([exn:fail? (lambda (er)
                          (define e-string (pretty-print-entity-string e))
                          (error (~a "Error ticking entity\n" e-string "\n" (exn-message er))))])

            (h g next-e c)))

        ;This gives a slowdown of about 4 FPS (per 1000 E)
        ;  But if the above is mutable, you don't have to do this one at all...
        #;
        (set! next-g (apply-op op next-g ei c))


        ;When putting these back in, maybe should require that dead and spawner be at the top two slots of the component list -- faster querying that way...  Or some kind of "dirty bit"
        #;
        (when (and (entity? op)
                   (get-component op dead?))
          (set! to-remove (cons e to-remove)))

        #;
        (when (and (entity? op)
                   (get-component op spawner?))
          (set! to-spawn (append (map spawner-to-spawn 
                                      (get-components op spawner?))
                                 to-spawn))
          (set! next-g (update-entity next-g op
                                      (curryr remove-component spawner?)
                                      )) 
          )
        
        (void)
        ))))

(define (handle-removals next-g)
  (for ([r to-remove])
    (when (debug-mode)
      (displayln "***REMOVING ENTITY***")
      (pretty-print-entity r))
    (set! next-g (remove-entity next-g r))))

(define (handle-spawns next-g)
  (for ([s to-spawn])
    (when (debug-mode)
      (displayln "***SPAWNING ENTITY***")
      (pretty-print-entity s))

    (set! next-g (add-entity next-g s))))




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

(define #;/contract 
  (has-id? e)
  #;
                 (-> entity? boolean?)
                 (number? (entity-id e)))

(define (all-entities pred?)
  (lambda (g)
    (andmap pred? (game-entities g))))

(define (apply-op o g ei c)
  (cond
    [(entity? o) (begin 
                   (update-entity g ei o))]
    [else (raise (~a "Unsupported handler return value: " o))]) )


(define (debug-hook:tick-begin g)
  (when (debug-mode)
    (displayln (~a "********TICK BEGIN*******"))
    (pretty-print-game g)))
