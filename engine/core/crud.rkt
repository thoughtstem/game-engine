#lang racket

(provide add-component
         add-components

         get-component
         get-components
         update-component
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


   (refresh-component-lookup
     (if (mutable-state)
       (begin
         (set-entity-components! e new-cs)
         e)
       (struct-copy entity e
                    [components new-cs])))
   
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

  (refresh-component-lookup
    (if (equal? real-new-c real-old-c) 
      e
      (if (mutable-state)
        (begin 
          (set-entity-components! e (list-set cs i real-new-c))

          (set-entity-changed?! e #t)

          e)
        (struct-copy entity e
                     [components (list-set cs i real-new-c)])))))


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

   (refresh-component-lookup
     (if (mutable-state)
       (begin
         (set-entity-components! e new-c) 
         e) 
       (struct-copy entity e
                    [components new-c])))) 


#;
(require memoize)

(define #;/memo 
  (get-component e query?)

  (if (symbol? query?)
    (hash-ref (entity-lookup e) query? #f) 
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

(define (has-component e c?)
  (findf c? (entity-components e)))

