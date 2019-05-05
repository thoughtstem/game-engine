#lang racket

(provide add-c remove-c #;update-c 
         add-e remove-e #;update-e 
         update-f e-crud? c-crud?)

(provide 
  game
  new-game
  game?
  game-entities
  set-game-entities!
  copy-game
  operation?

  entity
  new-entity
  entity?
  entity-components
  set-entity-components!
  set-entity-changed?!
  entity-changed?
  entity-id
  entity=?
  copy-entity

  new-component
  component
  copy-component
  component->list
  component?
  component-id
  component-update
  component=?
  component-done
  
  handler
  handler?
  noop
  rule?
  
  next-id
  mutable-state
  mutable!
  no-contracts!
  debug-mode
  maybe-contract

  CURRENT-ENTITY
  CURRENT-GAME

  get-value
  (struct-out despawn) 
  (struct-out spawn))

(require "./util.rkt"
         racket/contract/option )

(struct spawn (entity))
(struct despawn ())

(define CURRENT-ENTITY 
  (make-parameter #f))
(define CURRENT-GAME 
  (make-parameter #f))

(define mutable-state (make-parameter #f))
(define debug-mode    (make-parameter #f))
(define no-contracts  (make-parameter #f))


(define-syntax-rule (maybe-contract c)
  (option/c c #:with-contract (not (no-contracts))))


(define-syntax-rule (mutable! exp)
  (parameterize ([mutable-state #t])
    exp))

(define-syntax-rule (no-contracts! exp)
  (parameterize ([no-contracts #t])
    exp))

;Our basic struct types
(struct entity (id components changed?) #:mutable  #:transparent)
(struct game (entities) #:mutable #:transparent)

(struct add-e    (e))
;(struct update-e (e new-e))
(struct remove-e (e))

(struct add-c    (c))
;(struct update-c (c new-c))
(struct remove-c (c))

(struct update-f (c-id f-id func))

;Might not be necessary if operation? includes entity?
(define c-crud? (or/c add-c? #;update-c? 
                      remove-c?))

(define e-crud? (or/c add-e? 
                      remove-e?))
(define f-crud? update-f?)


(define (get-value c)
  (vector-ref c 5))

(define/contract (component? c)
  (-> any/c boolean?)
  (and (vector? c) 
       (eq? 'component (vector-ref c 0))))

(define operation? entity?)

(define handler? (-> game? entity? component? operation?))

(define-syntax-rule (handler (g e c) body ...)
  (lambda (g e c)
    body ...))



(define rule? (-> game? entity? component? boolean?))

(define noop (lambda (g e c) e))

;Commenting out this contract fixed a bug, but I don't know why... Be careful putting it back in.  However, it might be an easy bug to find once all the other contracts are back.  Tackle that next..
(define/contract (component id handler handler-code)
  (maybe-contract
    (->  (or/c number? #f) 
         (or/c handler? #f) 
         any/c
         component?))
  ;5 pre-fields before the user defined fields.
  (vector 'component #f id handler handler-code)) 


(define/contract (component-id c)
  (maybe-contract
    (-> component? (or/c number? #f)))

  (vector-ref c 2))

(define/contract (component-num-fields c)
  (maybe-contract
    (-> component? number?))              

   (- (vector-length c) 4))

(define/contract (component->list c)
   (-> component? (listof any/c))

   (drop (vector->list c) 4))

(define/contract (set-component-id c i)
  (maybe-contract
    (-> component? 
        (or/c  number? #f) ;Would you want it to be false?  Yes -- before the game has started, sometimes you call CRUD functions (update-component) that propagate along the #f to the newly constructed component.   
        component?))


  (if (mutable-state)
    (begin
      (vector-set! c 2 i)

      c) 
    (vector-copy c)))

(define/contract (component-subtype c)
  (maybe-contract
    (-> component? symbol?))

  (vector-ref c 1))

(define/contract (component-done c)
  (-> component? component?)
  (vector-set! c 3 #f)
  c)


;Contracts on this do affect FPS when there are lots of entities.
;  Gets called A LOT -- once per component in the game..
(define/contract (component-update c)
  (maybe-contract
    (-> component? 
        (or/c #f 
              (-> any/c any/c))))

  #;
  (define handlers (vector-ref c 3))

  (vector-ref c 3))

(define/contract (component=? c1 c2)
  (maybe-contract (-> component? component? boolean?))
  (eq? (component-id c1)
       (component-id c2)))

(define/contract (entity=? e1 e2)
  (maybe-contract
    (-> entity? entity? boolean?))

  (eq? (entity-id e1)
       (entity-id e2)))

(define-syntax-rule (new-component #:update update)
  (new-component-f #:update update
                   #:update-code 'update))

(define/contract (new-component-f #:update (update #f)
                                  #:update-code (update-code #f)
                                  )

  (maybe-contract
    (->* () 
         (#:update (or/c handler? #f)
          #:update-code any/c ) 
         component?))

  (component (next-id) 
             update
             #;
             (vector update)
             update-code))




(define/contract (copy-game g)
  (-> game? game?)
  (struct-copy game g))

(define/contract (copy-entity e)
  (-> entity? entity?)

  (apply new-entity
    (map copy-component (entity-components e)))
  
  )

(define/contract (copy-component c)
  (-> component? component?)

  (vector-copy c))


;Our basic constructors
;  Should the lists get converted to vectors?
;  Should this too be parameterizable?
(define/contract (new-game . es)
  (->* () #:rest (listof entity?) game?)
  (game (map copy-entity es)))

(define/contract (new-entity . cs)
  (->* () #:rest (listof component?) entity?)

  (entity (next-id) cs #f))

(define next-id (id-generator 0))





