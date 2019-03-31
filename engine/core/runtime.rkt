#lang racket

;How short can we make this file?
;The runtime should stay as lean and simple as possible

(provide initialize-game 
         
         tick

         all-entities
         has-id?
         next-entity-id)

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

      (define handler (component-handler->game-handler (component-handler c)))
      (define entity-handler (entity-handler->game-handler (component-entity-handler c)))
      (define game-handler (component-game-handler c))

      (set! temp-g (handler temp-g e c))
      (set! temp-g (entity-handler temp-g e c))
      (set! temp-g (game-handler temp-g e c))))
  
  temp-g)





(define/contract (has-id? e)
  (-> entity? boolean?)
  (number? (entity-id e)))

(define (all-entities pred?)
  (lambda (g)
    (andmap pred? (game-entities g))))

;Among other things, makes sure that the game's entity and component ids are all unique.  This is necessary for entity=? and component=?'s properties to hold.  That is, that the update CRUD operation maintains entity and component equality.  Running this on initialize-game ensures that the property holds at the beginning.  As long as the property holds after a call to (tick ...) then we have proven by induction that it always holds.
(define (initialize-game g)
  (init-ids g))

(define (init-ids g)
  (-> game? game?)
  (define es (map set-entity-id! (game-entities g)))   
  (game (map set-entity-components-ids! es)))

(define (set-entity-components-ids! e)
  (define new-e (struct-copy entity e))

  (define cs (map (lambda (c)
                    ((component-set-id c) c (next-entity-id)))
                  (entity-components new-e)))

  (struct-copy entity new-e
               [components cs]))


(define next-entity-id (id-generator 0))

(define/contract (set-entity-id! e)
  (-> entity? entity?)
  (struct-copy entity e
               [id (next-entity-id)]))



