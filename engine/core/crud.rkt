#lang racket

(provide ;Lifted versions
         add-component^
         update-component^
         remove-component^
  
         add-component
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
         get
         has-component
         get-field)

(require "./base.rkt"
         "./printer.rkt")


;COMPONENT CRUD


(define/contract (add-component e c)
  (maybe-contract
    (-> entity? component? entity?))

   (define new-cs
     (append 
       (entity-components e)
       (list c)))

   (if (mutable-state)
     (begin
       (set-entity-components! e new-cs)
       e)
     (struct-copy entity e
                  [components new-cs]))) 

(define/contract (add-component^ to-add)
  (maybe-contract
    (-> component? handler?))

   (lambda (g e c) 
     (add-component e to-add)))



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
    (if (mutable-state)
      (begin 
        (set-entity-components! e (list-set cs i real-new-c))

        (set-entity-changed?! e #t)

        e)
      (struct-copy entity e
                   [components (list-set cs i real-new-c)]))))


(define/contract (update-component^ to-update)
  (maybe-contract
    (-> (or/c component?
              (-> component? component?)) 
        handler?))

   (lambda (g e c) 
     (update-component e c to-update)))




(define/contract (remove-component e c)

  (maybe-contract
    (-> entity? 
        (or/c component?
              (-> component? boolean?)) 
        entity?))

   (define p (if (component? c)
                 (curry component=? c)
                 c))


   (define to-remove (findf p (entity-components e)))

   (define new-c
     (filter-not (curry component=? to-remove) 
                 (entity-components e)))

   (if (mutable-state)
     (begin
       (set-entity-components! e new-c) 
       e) 
     (struct-copy entity e
                [components new-c]))) 


(define/contract (remove-component^ to-remove)
  (maybe-contract
    (-> (or/c component?
              (-> component? boolean?))
        handler?))

   (lambda (g e c) 
     (remove-component e to-remove)))


(define (get-component e query?)
  (define real-query?
    (if (component? query?)
         (curry component=? query?)
         query?))

  (findf real-query? (entity-components e))
  )

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


(define (get-entity^ pred?-or-e)
  (lambda (g e c)
    (get-entity g pred?-or-e)))


(define/contract (add-entity g e)
  (maybe-contract
    (-> game? entity? game?))

  (define new-es (cons e (game-entities g)))

  (if (mutable-state)
    (begin
      (set-game-entities! g new-es)
      g)
    (game new-es)))


(define (add-entity^ to-add)
  (lambda (g e c)
    (add-entity g to-add)))



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


(define (update-entity^ old-e new-e)
  (lambda (g e c)
    (update-entity g old-e new-e)))



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

(define (remove-entity^ old-e)
  (lambda (g e c)
    (remove g old-e)))

;Useful query predicates that can be used in update-entity and get-entity


(define (has-component e c?)
  (findf c? (entity-components e)))


(define (get-field g entity-q? component-q? field-f)
  (field-f 
    (get-component 
      (entity-q? (game-entities g))
      component-q?)))




(define (get root . qs)
  (define orig-root root)
  (define orig-qs qs )
  (define (get-recur root . qs) 
    (cond 
      [(not root) (error "Couldn't get: " orig-root orig-qs)  ]
      [(empty? qs) root]
      [(game? root) 
       (apply get-recur
              (get-entity root (first qs))
              (rest qs))]
      [(entity? root) 
        (apply get-recur 
                (get-component root (first qs))
                (rest qs))]
      [(component? root) 
       ((first qs) root)]))

  (define ret
    (apply get-recur root qs))

  ret)






