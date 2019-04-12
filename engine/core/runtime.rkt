#lang racket

(provide init 
         tick
         ticks

         tick-list

         all-entities
         has-id?)

(require "./base.rkt"
         "./crud.rkt"
         "./util.rkt"
         "./handler-util.rkt"
         "./spawner.rkt")

(define debug-mode (make-parameter #f))

(define/contract (tick g)
  (-> game? game?)

  ;But maybe not in the runtime... maybe as a wrapper...
  (displayln "TICK -- TODO: MAKE A DEBUG MODE")

  (define next-g
    (struct-copy game g))

  (define to-remove '())
  (define to-spawn  '())
  (for ([e (game-entities g)])
    (for ([c (entity-components e)])
      (define h (component-handler c))

      (define next-e (get-entity next-g e))  
      (define next-c (get-component next-e c))  

      (when h 
        ;A handler gets to see the last game state and the current entity/component state
        (define op (h g next-e next-c))

        ;Apply the op to the game
        (set! next-g (apply-op op next-g next-e next-c))

        (when (and (entity? op)
                   (get-component op dead?))
          (set! to-remove (cons e to-remove)))

        (when (and (entity? op)
                   (get-component op spawner?))
          (set! to-spawn (append (map spawner-to-spawn 
                                      (get-components op spawner?))
                                to-spawn))
          (set! next-g (update-entity next-g op
                                     (curryr remove-component spawner?)
                                     )) 
          ))))

  ;Could just foldl, but we've already done a for loop, so I'll just keep the style consistent
  (for ([r to-remove])
    (displayln "Removing.  TODO: THROW AN ERROR IF SOMETHING DIES TWICE.  OR A WARNING?")
    (set! next-g (remove-entity next-g r)))

  (for ([s to-spawn])
    (set! next-g (add-entity next-g s)))

  next-g)

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

(define/contract (has-id? e)
  (-> entity? boolean?)
  (number? (entity-id e)))

(define (all-entities pred?)
  (lambda (g)
    (andmap pred? (game-entities g))))

;Among other things, makes sure that the game's entity and component ids are all unique.  This is necessary for entity=? and component=?'s properties to hold.  That is, that the update CRUD operation maintains entity and component equality.  Running this on initialize-game ensures that the property holds at the beginning.  As long as the property holds after a call to (tick ...) then we have proven by induction that it always holds.
