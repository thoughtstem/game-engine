#lang racket

(provide 
  game
  new-game
  game?
  game-entities
  copy-game
  game-op?

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
  component-entity-handler
  component-game-handler
  component=?
  
  entity-handler?
  component-handler?
  
  set-ids!
  
  done?
  noop?
  noop
  done
  apply-script
  init-script

  game-handler-script-generator?
  )

(require "./util.rkt")

;Our basic struct types
(struct entity (id components)  #:transparent)
(struct game (entities) #:transparent)

(define/contract (component? c)
  (-> any/c boolean?)
  (and (vector? c) 
       (eq? 'component (vector-ref c 0))))

(define game-op? (or/c game? #f 'done))

;Our basic function types
(define game-handler? (-> game? entity? component? game-op?))
(define entity-handler? (-> entity? component? entity?))
(define component-handler? (-> component? component?))

(define game-handler-script? 
  (-> game? entity? component? game-op?))

(define game-handler-script-generator?
  (or/c game-handler-script?
        (-> game-handler-script?)))


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


(define/contract (component-handler c)
  (-> component? component-handler?)
  (vector-ref (vector-ref c 3) 0))

(define/contract (component-entity-handler c)
  (-> component? entity-handler?)
  (vector-ref (vector-ref c 3) 1))

(define/contract (component-game-handler c)
  (-> component? game-handler-script-generator?)
  (vector-ref (vector-ref c 3) 2))


(define/contract (component=? c1 c2)
  (-> component? component? boolean?)
  (eq? (component-id c1)
       (component-id c2)))

(define/contract (entity=? e1 e2)
  (-> entity? entity? boolean?)
  (eq? (entity-id e1)
       (entity-id e2)))

(define/contract 
  (new-component #:handler        (handler (lambda (c) c))
                 #:entity-handler (entity-handler (lambda (e c) e))
                 #:game-handler   (game-handler (lambda (g e c) g)))
  (->* () (#:handler component-handler?
           #:entity-handler entity-handler?
           #:game-handler game-handler?) component?)
  (component #f 
             (vector handler entity-handler game-handler)))




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
  (eq? s #f))

(define (noop)
  (const #f))

(define (done)
  (const 'done))



(define (init-script s)
  (if (and (procedure? s)
           (= 0 (procedure-arity s))) ;Cuz I don't think we can use game-handler-script-generator? to detect if s is a script generator...  So arity hack is the best I can think of...
    (s)
    s))

(define/contract (apply-op g op)
                 (-> game? game-op? game?)               
                 (if (game? op)
                   op ;If it's not #f or 'done, it's the new state.
                   g  ;Else no change.  In the end 'done is an optimization that stops calling the script.
                   ))

(define/contract (apply-script g e c s)
   (-> game? entity? component? game-handler-script-generator? game?)
   (define real-s (init-script s))

   (define op (real-s g e c))
   (apply-op g op)) 

(define (init g)
  (init-ids g))

(define (init-ids g)
  (-> game? game?)
  (game (map set-ids! (game-entities g))))
