#lang racket

(provide init 
         tick
         tick!
         debug-tick
         debug
         ticks
         tick-component
         tick-entity

         tick-list

         all-entities
         has-id?
         display-performance-stats
         profile)

(require "./base.rkt"
         "./crud.rkt"
         "./util.rkt"
         "./printer.rkt"
         "./debug.rkt")



(define to-remove '())
(define to-spawn  '())

(define (tick! g)
  (mutable! (tick g)))

(define component-times #f)
(define profiler-on (make-parameter #f))

(define-syntax-rule (profile exp)
  (parameterize ([profiler-on #t])
    exp))

(define/contract (tick g)
  (maybe-contract
    (-> game? game?))


  (when profiler-on
    (set! component-times (make-hash)))

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
            (define component-start-time (current-inexact-milliseconds))

            (define new-c (h c))  
            (update-component! e c new-c)

            (when (profiler-on)
              (hash-update!
                component-times
                (cons ei (vector-ref c 1))
                (lambda (t)
                  (+ t
                     (- (current-inexact-milliseconds) component-start-time)))
                (- (current-inexact-milliseconds) component-start-time)))

            (define v (get-value new-c)) 

            (when (despawn? v)
              (set! to-remove (cons next-e to-remove))
              (debug:added-to-remove-queue to-remove))

            (when (spawn? v)
              (set! to-spawn (cons (spawn-entity v) to-spawn))
              (debug:added-to-spawn-queue to-spawn) )
            
            (debug:component-tick-end new-c)  
            ) 

          ))
    


      (when (not (mutable-state))
        (set-game-entities! g
                            (list-set (game-entities g) ei next-e)))) 


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

;Does this get used?  Is it old?
(define/contract (has-id? e)
  (maybe-contract
    (-> entity? boolean?))
                 (number? (entity-id e)))

(define (all-entities pred?)
  (lambda (g)
    (andmap pred? (game-entities g))))

(define (debug-tick g)
  (debug
    (tick g)))



(define (display-performance-stats)
  #;
  (displayln "Performance stats disabled")

  (when component-times
    (define data (hash->list component-times))

    (define grouped-by-type
      (let ([h (make-hash)])
        (for ([d data])
          (define type (cdr (car d)))
          (define time (cdr d))
          (hash-update! h 
                        type
                        (lambda (t)
                          (+ t time) )
                        time))
        h))

    (define sorted
      (sort data < #:key cdr))

    (define total
      (apply + (map cdr data)))

    (define top
      (if (< (length sorted) 5)
        sorted
        (take sorted 5)))


    (pretty-print
      (map
        (lambda (t)
          (list (car t)
                (/ (cdr t) total)))
        (sort
          (hash->list grouped-by-type)
          <
          #:key cdr))

      )))

(define (tick-component c)
  (define h (component-update c)) 
  (if h (h c) c))

(define (tick-entity e)
  (parameterize ([CURRENT-ENTITY e])
    (for ([c (entity-components e)])
      (update-component! e c (tick-component c)))

    e))


