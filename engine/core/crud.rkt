#lang racket

(provide add-component
         add-components

         get
         get-component
         get-components
         update-component
         update-component!
         remove-component
         
         ;Entity CRUD
         add-entity
         get-entity
         update-entity
         remove-entity

         ;Query language
         has-component
         )

(require "./base.rkt"
         "./debug.rkt"
         "./printer.rkt")


;COMPONENT CRUD


(define/contract (add-component e c)
  (maybe-contract
    (-> entity? component? entity?))

   (define new-cs
     (append 
       (entity-components e)
       (if (list? c)
         c
         (list c))))

   (set-entity-components! e new-cs)
   ) 

(define (add-components e . cs)
  (if (empty? cs)
    e
    (apply add-components (add-component e (first cs)) 
           (rest cs))))

(define/contract (update-component e old-c new-c)

  (maybe-contract
    (-> entity? 
        (or/c component? (-> component? boolean?)) 
        (or/c component? (-> component? component?))
        entity?))

  (define cs (entity-components e))

  (define i  (cond 
               [(number? old-c) old-c]
               [(component? old-c) (index-of cs old-c component=?)]
               [(procedure? old-c) (index-where cs old-c)]))

  (when (not i)
    (define e-string
      (with-output-to-string
        (thunk
          (pretty-print-entity e))))

    (if (component? old-c)
      (error (~a "update-component: could not find a component matching the id of " old-c "\n" e-string))
      (error (~a "update-component: could not find a component matching the query " old-c "\n" e-string))))

  (define real-old-c (list-ref cs i))

  (define action (if (component? new-c)
                     (thunk* 
                       new-c) 
                     new-c))

  (define real-new-c 
    (action real-old-c))

  (if (equal? real-new-c real-old-c) 
    e
    (begin 
      ;Is the list-set here slow???
      ; An entity's components should definitely be a vector (or a hash!), since we never add/remove at runtime.  Then the component list can just 
      (set-entity-components! e (list-set cs i real-new-c))
      (set-entity-changed?! e #t)

      e)
    )
  )


(define (update-component! e c-old c-new)
  (hash-set! (entity-component-hash e)
             (component-name c-old)
             c-new))


(define/contract (remove-component e c)
  (maybe-contract
    (-> entity? 
        (or/c component?
              (-> component? boolean?)) 
        entity?))

   (define p (if (component? c)
                 (curry component=? c)
                 c))


   (define to-remove 
           (findf p (entity-components e)))

   (when (not to-remove)
     (displayln c)
     (pretty-print-entity e)
     (error "remove-component: No component matching query."))
   

   (define new-c
     (filter-not (curry component=? to-remove) 
                 (entity-components e)))

     (set-entity-components! e new-c))


(define 
  ;Leaving the contract disabled for now.  Has a 10 FPS performance boost in bullet-cloud.rkt test.  (For some reason no-contracts! isn't giving the same boost...)

  #;/contract 
  (get-component e query?)

  #;
  (maybe-contract
    (-> entity? any/c any/c))

  (if (symbol? query?)
    (hash-ref (entity-component-hash e) query? #f) 
    (let ([real-query?
            (if (component? query?)
              (curry component=? query?)
              query?)])

      (findf real-query? (entity-components e)))))

(define (get-components e query?)
  (define real-query?
    (if (component? query?)
         (curry component=? query?)
         query?))

  (filter real-query? (entity-components e)))



;ENTITY CRUD
(define (get-entity-index g pred?-or-e)
  (define es (game-entities g))

  (define i  (if (entity? pred?-or-e)
                 (index-of es pred?-or-e entity=?)
                 (index-where es pred?-or-e)))
  i)


(define/contract (get-entity g pred?-or-e)

  (maybe-contract
    (-> game? (or/c entity? 
                    (-> entity? any/c)) 
        (or/c entity? #f)))

  (define es (game-entities g))
  (define i  (get-entity-index g pred?-or-e))
  (if i (list-ref es i) #f))


(define/contract (add-entity g e)
  (maybe-contract
    (-> game? entity? game?))

  (define new-es (cons e (game-entities g)))

  (if (mutable-state)
    (begin
      (set-game-entities! g new-es)
      g)
    (game new-es)))

(define/contract (update-entity g old-e new-e)
  (maybe-contract
    (-> game? (or/c number? entity? (-> entity? boolean?)) 
        (or/c entity? 
              (-> entity? entity?))
        game?))

  (define es (game-entities g))
  (define i  (if (number? old-e) 
               old-e
               (get-entity-index g old-e)))

  (define action (if (entity? new-e)
                     (thunk* new-e)
                     new-e))


  (define new-es
    (list-set es i (action (list-ref es i))))

  (if (mutable-state)
    (begin
      (set-game-entities! g new-es)
      g)
      (struct-copy game g [entities new-es])))


(define/contract (remove-entity g old-e)
  (maybe-contract
    (-> game? (or/c entity? 
                    (-> entity? boolean?)) 
        game?))

  (define es (game-entities g))
  (define p  (if (entity? old-e)
                 (curry entity=? old-e)
                 old-e))

  (define to-remove (findf p es))

  (define new-es (remove to-remove es entity=?))

  (if (mutable-state)
    (begin
      (set-game-entities! g new-es) 
      g)
    (struct-copy game g
                [entities new-es])))

(define/contract (has-component e c?)
  (maybe-contract
    (-> entity? any/c boolean?))

  (not (not
         (if (symbol? c?)
           (hash-ref (entity-component-hash e) c? #f) 
           (findf c? (entity-components e))))))

(define (get entity-name component-name)
  (define entity-with-name
    (get-entity (CURRENT-GAME) 
                (lambda (e)
                  (and
                    ;A bit hacky, since name is defined outside of core.  TODO: Should rope that component into core if it has special meaning here.
                    (has-component e 'name)
                    (eq? entity-name 
                         (get-value (get-component e 'name)))))))

  (when (not entity-with-name)
    (pretty-print-game (CURRENT-GAME))
    (error (~a "No entity with name " entity-name " and component name " component-name)))


  (get-value
    (get-component 
      entity-with-name 
      component-name)))




