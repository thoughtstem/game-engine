#lang racket

(provide 
  game
  new-game
  game?
  game-entities

  entity
  new-entity
  entity?
  entity-components
  entity-id
  entity=?

  new-component
  component
  component?
  component-id
  component-set-id
  component-handler
  component-entity-handler
  component-game-handler
  component=?
  
  entity-handler?
  component-handler?)

;Our basic struct types
(struct component (id set-id handler entity-handler game-handler)  #:transparent)
(struct entity (id components)  #:transparent)
(struct game (entities) #:transparent)

;Our basic function types
(define game-handler? (-> game? entity? component? game?))
(define entity-handler? (-> entity? component? entity?))
(define component-handler? (-> component? component?))

(define/contract (component=? c1 c2)
  (-> component? component? boolean?)
  (eq? (component-id c1)
       (component-id c2)))

(define/contract (entity=? e1 e2)
  (-> entity? entity? boolean?)
  (eq? (entity-id e1)
       (entity-id e2)))

;Our basic constructors
(define/contract (new-game . es)
  (->* () #:rest (listof entity?) game?)
  (game es)) 
(define/contract (new-entity . cs)
  (->* () #:rest (listof component?) entity?)
  (entity #f cs))

(define/contract 
  (new-component #:handler        (handler (lambda (c) c))
                 #:entity-handler (entity-handler (lambda (e c) e))
                 #:game-handler   (game-handler (lambda (g e c) g)))
  (->* () (#:handler component-handler?
           #:entity-handler entity-handler?
           #:game-handler game-handler?) component?)
  (component #f (lambda (c i)
                  (struct-copy component c
                               [id i])) 
             handler entity-handler game-handler))




