#lang racket

(provide init 
         tick
         tick-list

         all-entities
         has-id?)

(require "./base.rkt"
         "./crud.rkt"
         "./util.rkt"
         "./handler-util.rkt")

(define/contract (tick g)
  (-> game? game?)

  (define temp-g
    (struct-copy game g))

  (for ([e (game-entities g)])
    (for ([c (entity-components e)])

      (define handler (component-handler c))

      (when handler
        (set! temp-g (apply-script temp-g e c handler)))

      ))
  
  temp-g)

(define (apply-script g e c h)
  (define o (h g e c)) 

  (apply-op o g e c))

(define (apply-op o g e c)
  (cond
    [(game? o) o]
    [(entity? o) (update-entity g e o)]
    [(component? o) (update-entity g e 
                                   (update-component e o))]
    [(noop? o) g]
    [(done? o) (update-entity g e 
                                (component-done c))]
    [(list? o) (foldl (lambda (next-op g)
                        (apply-op next-op g)) 
                      g o)]
    [else (raise (~a "Unsupported handler return value: " o))]))

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
