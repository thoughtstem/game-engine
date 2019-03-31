#lang racket

(provide ;Component CRUD
         update-component
         get-component
         add-component
         remove-component
         
         ;Entity CRUD
         get-entity
         update-entity

         ;Query language
         has-component)

(require "./base.rkt")


;COMPONENT CRUD

(define/contract (add-component e c)
   (-> entity? component? entity?)
   (struct-copy entity e
                [components (append (entity-components e) 
                                    (list c))])) 

(define/contract (update-component e old-c new-c)
  (-> entity? (or/c component? procedure?) (or/c component? procedure?) entity?)

  (define cs (entity-components e))
  (define i  (if (component? old-c)
                 (index-of cs old-c component=?)
                 (index-where cs old-c)))

  (define action (if (component? new-c)
                     (thunk* new-c)
                     new-c))

  (struct-copy entity e
               [components (list-set cs i (action (list-ref cs i)))]))


(define/contract (remove-component e c)
   (-> entity? (or/c procedure? component?) entity?)

   (define p (if (component? c)
                 (curry component=? c)
                 c))

   (struct-copy entity e
                [components (filter-not p (entity-components e))])) 

(define (get-component e pred?)
  (findf pred? (entity-components e)))



;ENTITY CRUD
(define (get-entity-index g pred?-or-e)
  (define es (game-entities g))

  (define i  (if (entity? pred?-or-e)
                 (index-of es pred?-or-e entity=?)
                 (index-where es pred?-or-e)))
  i)


(define (get-entity g pred?-or-e)
  (-> game? (or/c entity? procedure?) entity?)

  (define es (game-entities g))
  (define i  (get-entity-index g pred?-or-e))
  (if i (list-ref es i) #f))



(define (update-entity g old-e new-e)
  (-> game? (or/c entity? procedure?) (or/c entity? procedure?) game?)

  (define es (game-entities g))
  (define i  (get-entity-index g old-e))

  (define action (if (entity? new-e)
                     (thunk* new-e)
                     new-e))

  (struct-copy game g
               [entities (list-set es i (action (list-ref es i)))]) )



;Useful query predicates that can be used in update-entity and get-entity

(define (has-component c?)
  (lambda (e)
    (findf c? (entity-components e))))

