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
  component->list
  component?
  component-id
  component-update
  component=?
  component-done
  
  handler?
  noop
  rule?
  
  next-id)

(require "./util.rkt")

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


(define/contract (component? c)
  (-> any/c boolean?)
  (and (vector? c) 
       (eq? 'component (vector-ref c 0))))

(define operation? entity?)

(define handler? (-> game? entity? component? operation?))
(define rule? (-> game? entity? component? boolean?))

(define noop (lambda (g e c) e))

;Component can be a bit more light weight
(define/contract (component id handler)
  (->  (or/c number? #f) 
       (or/c handler? #f) component?)
  (vector 'component #f id handler)) 

(define #;/contract 
  (component-id c)
  #;(-> component? (or/c number? #f))
  (vector-ref c 2))

(define #;/contract 
  (component-num-fields c)
   ;(-> component? number?)              
   (- (vector-length c) 4))

(define/contract (component->list c)
   (-> component? (listof any/c))

   (drop (vector->list c) 4))

(define #;/contract 
  (set-component-id c i)
  #;
  (-> component? 
      (or/c  number? #f) ;Would you want it to be false?  Yes -- before the game has started, sometimes you call CRUD functions (update-component) that propagate along the #f to the newly constructed component.   
      component?)

  #;
  (define new-c (vector-copy c))
  
  (vector-set! c 2 i)
  
  c
  
  )

(define #;/contract 
  (component-subtype c)

  #;
  (-> component? symbol?)
  (vector-ref c 1))

(define/contract (component-done c)
  (-> component? component?)
  (vector-set! c 3 #f)
  c)


;Contracts on this do affect FPS when there are lots of entities.
;  Gets called A LOT -- once per component in the game..
(define #;/contract 
  (component-update c)
  #;(-> component? (or/c #f handler?))

  #;
  (define handlers (vector-ref c 3))

  (vector-ref c 3))

(define #;/contract 
  (component=? c1 c2)
 #; (-> component? component? boolean?)
  (eq? (component-id c1)
       (component-id c2)))

(define #;/contract 
  (entity=? e1 e2)
  #;
  (-> entity? entity? boolean?)
  (eq? (entity-id e1)
       (entity-id e2)))

(define/contract (new-component #:update (update #f))
  (->* () 
       (#:update (or/c handler? #f)) 
       component?)
  (component (next-id) 
             update
             #;
             (vector update)))




(define/contract (copy-game g)
  (-> game? game?)
  (struct-copy game g))

(define/contract (copy-entity e)
  (-> entity? entity?)
  (struct-copy entity e))



;Our basic constructors
(define/contract (new-game . es)
  (->* () #:rest (listof entity?) game?)
  (game es))

(define/contract (new-entity . cs)
  (->* () #:rest (listof component?) entity?)
  (entity (next-id) cs #f))

(define next-id (id-generator 0))





