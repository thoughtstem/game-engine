#lang racket

(provide init 
         tick
         ticks
         tick-component

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
  (for ([e (game-entities g)]
        [ei (in-naturals)])
    (for ([c (entity-components e)]
          [ci (in-naturals)])

      (define h (component-update c))

      ;Entities don't change positions in the list in mid tick, so we can always get the newest version of e from next-g
      (define next-e (list-ref (game-entities next-g) ei))

      (when h 
        ;A handler gets to see the last game state and the current entity state, and the last component state (redundant, but for convenience...)
        (define op (h g next-e c))

        ;Apply the op to the game
        (set! next-g (apply-op op next-g ei c))

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

(define (tick-component g e c)
   (define h (component-update c))
   (h g e c))

(define/contract (has-id? e)
  (-> entity? boolean?)
  (number? (entity-id e)))

(define (all-entities pred?)
  (lambda (g)
    (andmap pred? (game-entities g))))

(define (apply-op o g ei c)
  (cond
    [(component? o) (begin 
                      (update-entity g
                                     ei
                                     (update-component 
                                       (list-ref (game-entities g) ei) 
                                       c o)))]
    [(entity? o) (begin 
                   (update-entity g ei o))]
    [else (raise (~a "Unsupported handler return value: " o))]) )


