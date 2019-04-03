#lang racket

(provide ;Component CRUD
         update-component
         get-component
         add-component
         remove-component
         
         ;Entity CRUD
         get-entity
         update-entity
         add-entity
         remove-entity

         ;Query language
         has-component)

(require "./base.rkt")


;COMPONENT CRUD

(define/contract (add-component e c)
   (-> entity? component? entity?)
   (struct-copy entity e
                [components (cons c (entity-components e) )])) 

(define/contract (update-component e old-c new-c)
  (-> entity? 
      (or/c component? (-> component? boolean?)) 
      (or/c component? (-> component? component?)) 
      entity?)

  (define cs (entity-components e))

  (define i  (if (component? old-c)
                 (index-of cs old-c component=?)
                 (index-where cs old-c)))

  (define real-old-c (list-ref cs i))

  (define action (if (component? new-c)
                     (thunk* 
                       new-c) 
                     new-c))

  (define real-new-c 
    (set-component-id    ;Make sure users cannot accidentally update a component's id
      (action real-old-c)
      (component-id real-old-c)))

  (struct-copy entity e
               [components (list-set cs i real-new-c)]))


(define/contract (remove-component e c)
   (-> entity? 
       (or/c component?
             (-> component? boolean?)) 
       entity?)

   (define p (if (component? c)
                 (curry component=? c)
                 c))


   (define to-remove (findf p (entity-components e)))

   (struct-copy entity e
                [components (filter-not (curry component=? to-remove) 
                                        (entity-components e))])) 

(define (get-component e query?)
  (define real-query?
    (if (component? query?)
         (curry component=? query?)
         query?))

  (findf real-query? (entity-components e)))



;ENTITY CRUD
(define (get-entity-index g pred?-or-e)
  (define es (game-entities g))

  (define i  (if (entity? pred?-or-e)
                 (index-of es pred?-or-e entity=?)
                 (index-where es pred?-or-e)))
  i)


(define/contract (get-entity g pred?-or-e)
  (-> game? (or/c entity? 
                  (-> entity? any/c)) entity?)

  (define es (game-entities g))
  (define i  (get-entity-index g pred?-or-e))
  (if i (list-ref es i) #f))

(define (add-entity g e)
  (-> game? entity? game?)

  (game (cons (set-ids! e) ;Ensure added entities and their components start with unique ids
              (game-entities g))))


(define (update-entity g old-e new-e)
  (-> game? (or/c entity? (-> entity? boolean?)) 
            (or/c entity? 
                  (-> entity? entity?))
            game?)

  (define es (game-entities g))
  (define i  (get-entity-index g old-e))

  (define action (if (entity? new-e)
                     (thunk* new-e)
                     new-e))

  (struct-copy game g
               [entities (list-set es i (action (list-ref es i)))]) )


(define (remove-entity g old-e)
  (-> game? (or/c entity? 
                  (-> entity? boolean?)) 
      game?)

  (define es (game-entities g))
  (define p  (if (entity? old-e)
                 (curry entity=? old-e)
                 old-e))

  (define to-remove (findf p es))

  (struct-copy game g
               [entities (remove to-remove es entity=?)]) )

;Useful query predicates that can be used in update-entity and get-entity

(define (has-component c?)
  (lambda (e)
    (findf c? (entity-components e))))

