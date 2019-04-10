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
         "./handler-util.rkt")

(define/contract (tick g)
  (-> game? game?)

  (define temp-g
    (struct-copy game g))

  (for ([e (game-entities g)])
    ;Should make a temp-g here
    (for ([c (entity-components e)])
      (define h (component-handler c))

      (when h

        ;Should pass in temp-e here
        ;Handlers get to see the last tick's version of g,
        ;  But the mid-tick's version of e.
        ;What about c??  
        ;  If we grab from temp-e, then it's the mid-tick version (Feels right, I think...)
        ;  If we use c, then it's the last tick's version.
        ;  When does it matter?
        (define op (h g e c))

        ;If op is an entity operation, update temp-e too
        (set! temp-g (apply-op op temp-g e c))
        
        )))

  temp-g)

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
