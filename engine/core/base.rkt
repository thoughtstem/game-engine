#lang racket

(provide 
  game
  new-game
  game?
  game-entities
  set-game-entities!
  copy-game

  entity
  entity-component-hash
  check-for-duplicate-components 
  new-entity
  entity?
  entity-components
  set-entity-components!
  set-entity-changed?!
  entity-changed?
  entity-id
  entity-id=?
  entity=?
  copy-entity

  component
  copy-component
  component?
  component-id
  component-update
  component=?
  component-done
  component-name
  
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
(struct entity (id component-hash component-names changed? ) #:mutable  #:transparent)
(struct game (entities) #:mutable #:transparent)

(define (entity-components e)
  (define names (entity-component-names e))  

  (map 
    (lambda (n)
      (hash-ref (entity-component-hash e) n))  
    names))

(define (get-value c)
  (vector-ref c 5))


(define/contract (component? c)
  (-> any/c boolean?)
  (and (vector? c) 
       (eq? 'component (vector-ref c 0))))

(define/contract (component id handler handler-code)
  (maybe-contract
    (->  (or/c number? #f) 
         any/c
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

(define/contract (component-name c)
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
 (->* () #:rest (listof (or/c entity?
                              list?)) game?)
  (game (map copy-entity (flatten es))))

(define/contract (new-entity . cs)
 (->* () #:rest (listof (or/c component?
                              list?)) entity?)

 (define flat-cs (flatten cs))

 (check-for-duplicate-components
   (entity (next-id) 
           (components->component-hash flat-cs)
           (map component-name flat-cs)
           #f))
)

(define (set-entity-components! e cs)
 (check-for-duplicate-components
   (entity (entity-id e) 
           (components->component-hash (flatten cs))
           (map component-name cs)
           #f)))

(define (components->component-hash cs)
  (define ret (make-hasheq)) 

  (for ([c cs])
    (hash-set! ret 
               (component-name c)
               c))

  ret)


(define (check-for-duplicate-components e)
  (define cs (entity-components e))

  (when (duplicate-names? cs)
    (displayln cs)
    ;TODO: Make it tell you which component?
    (error "You shouldn't have more than one component with the same name."))
  
  e)


(define next-id (id-generator 0))


(define (duplicate-names? cs)
  (define names 
    (map component-name (flatten cs)))

  (not (= (length (remove-duplicates names))
          (length names))))



(define (entity-id=? e i)
  (= (entity-id e) i))



