#lang racket

(provide 
  game
  new-game
  game?
  game-entities
  copy-game
  operation?

  entity
  new-entity
  entity?
  entity-components
  entity-id
  entity=?
  copy-entity

  new-component
  component
  component?
  component-id
  set-component-id
  component-handler
  component=?
  component-done
  
  
  set-ids!
  
  done?
  noop?
  handler?
  lift-to-handler
  
  init)

(require "./util.rkt")

;Our basic struct types
(struct entity (id components)  #:transparent)
(struct game (entities) #:transparent)


(define/contract (component? c)
  (-> any/c boolean?)
  (and (vector? c) 
       (eq? 'component (vector-ref c 0))))

(define operation? 
  (or/c game? entity? component? 'noop 'done
        (listof (or/c game? entity? component? 'noop 'done))))

(define handler? (-> game? entity? component? operation?))


;Component can be a bit more light weight
(define/contract (component id handlers)
  (->  (or/c number? #f) vector? component?)
  (vector 'component #f id handlers)) 

(define/contract (component-id c)
  (-> component? (or/c number? #f))
  (vector-ref c 2))

(define/contract (set-component-id c i)
  (-> component? number? component?)

  (define new-c (vector-copy c))
  
  (vector-set! new-c 2 i)
  
  new-c)

(define/contract (component-subtype c)
  (-> component? symbol?)
  (vector-ref c 1))

(define/contract (component-done c)
  (-> component? component?)
  (vector-set! c 3 #f)
  c)


(define/contract (component-handler c)
  (-> component? (or/c #f handler?))
  (define handlers (vector-ref c 3))
  (if handlers
    (vector-ref handlers 0)
    #f))

(define/contract (component=? c1 c2)
  (-> component? component? boolean?)
  (eq? (component-id c1)
       (component-id c2)))

(define/contract (entity=? e1 e2)
  (-> entity? entity? boolean?)
  (eq? (entity-id e1)
       (entity-id e2)))

(define/contract (new-component #:update (update #f))
  (->* () 
       (#:update handler?) 
       component?)
  (component #f (vector update)))




(define/contract (copy-game g)
  (-> game? game?)
  (struct-copy game g))

(define/contract (copy-entity e)
  (-> entity? entity?)
  (struct-copy entity e))



;Our basic constructors
(define/contract (new-game . es)
  (->* () #:rest (listof entity?) game?)
  (init (game es))) 
(define/contract (new-entity . cs)
  (->* () #:rest (listof component?) entity?)
  (entity #f cs))

(define/contract (set-ids! e)
  (-> entity? entity?)

  (set-entity-components-ids!
    (set-entity-id! e)))

(define/contract (set-entity-id! e)
  (-> entity? entity?)
  (struct-copy entity e
               [id (next-entity-id)]))

(define (set-entity-components-ids! e)
  (define new-e (struct-copy entity e))

  (define cs (map (lambda (c)
                    (set-component-id c (next-entity-id)))
                  (entity-components new-e)))

  (struct-copy entity new-e
               [components cs]))

(define next-entity-id (id-generator 0))


(define (done? s)
  (eq? s 'done))

(define (noop? s)
  (eq? s 'noop))


(define (init g)
  (init-ids g))

(define (init-ids g)
  (-> game? game?)
  (game (map set-ids! (game-entities g))))


           
(define/contract (lift-to-handler c->c)
  (-> (or/c #f
            (-> entity? component? entity?)
            (-> component? component?)
            handler?) 
      (or/c #f handler?))

  (cond
    [(not c->c) #f]
    [(= 1 (procedure-arity c->c))
     (lambda (g e c)
       (c->c c))]
    [(= 2 (procedure-arity c->c))
     (lambda (g e c)
       (c->c e c))]
    [else c->c]))


