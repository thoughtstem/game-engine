#lang racket

;TODO: Need to document the big four CRUDs: update-component, get-component, add-component, and remove-component.   Especially the predicate behaviour (remove all? remove first?) (update all?  update first?).  Are we overloading the function too much?  Should we break the functionality out into different functions?

;TODO: 
;  Sprite management is the big one...
;  Big steps: Get rendering working
;             Get physics working

;TODO: side quest.  Make sound toggleable...

(provide entity?
         entity-components
         (rename-out [new-entity entity])
         game?
         game-entities
         (rename-out [new-game game])
         component
         component-handler
         define-component

         update-component
         get-component
         add-component
         remove-component

         init-ids
         all-entities
         has-id?
         tick
         tick-entity
         
         entity-handler?
         component-handler?)


(require (for-syntax racket))
(require (for-syntax racket/syntax))

(struct component (id handler entity-handler)  #:transparent)
(struct entity (id components)  #:transparent)
(struct game (entities) #:transparent)

(define entity-handler? (-> entity? component? entity?))

(define (specific-entity-handler? component-type?) 
  ;I'm weirdly amazed that this works.  Am I missing something?
  (-> entity? component-type? entity?))

(define component-handler? (-> component? component?))

(define/contract (new-game . es)
  (->* () #:rest (listof entity?) game?)
  (game es)) 
(define/contract (new-entity . cs)
  (->* () #:rest (listof component?) entity?)
  (entity #f cs))

(define-syntax (generate-handlers stx)
  (syntax-case stx ()
    [(_ name extra-name field)
     (with-syntax 
       [ (name? (format-id #'name "~a?" #'name))

         ;e.g. update-health-amount
         (update-component-field (format-id #'name "update-~a-~a" #'name #'field))
         ;e.g. update-entity-health-amount
         (update-entity-component-field (format-id #'name "update-entity-~a-~a" #'name #'field))

         ;e.g. update-entity-health
         (update-entity-component (format-id #'name "update-entity-~a" #'name))

         ;e.g. update-entity-first-health
         (update-entity-first-component (format-id #'name "update-entity-first-~a" #'name))
         ;e.g. health-amount
         (getter (format-id #'name "~a-~a" #'name #'field))]
       #`(begin

           (define (update-component-field f)
             (lambda (c)
               (struct-copy name c
                            [field (f (getter c))])))

           (define (update-entity-component-field f)
             (lambda (e c)
               (update-component e c (update-component-field f))))
           
           (define (update-entity-component c2)
             (lambda (e c)
               (update-component e c c2)))

           (define (update-entity-first-component c2)
             (lambda (e c)
               (update-component e name? c2)))

           ) )]))


(define-syntax (define-component stx)
  (syntax-case stx ()
    [(_ name (field ...))
     (with-syntax [(other-constructor (format-id #'name "new-~a" #'name))
                   (name-copy (format-id #'name "~a-copy" #'name)) 
                   (extra-name (format-id #'name "extra-~a" #'name))
                   (name? (format-id #'name "~a?" #'name))
                   (anys  (map (thunk* #'any/c) 
                               (syntax->datum #'(field ...)))) ] 
        #`(begin
             (struct name component (field ...) #:transparent
               #:extra-name extra-name)

             (generate-handlers name extra-name field)
             ...

             (define/contract (other-constructor #:handler (handler (lambda (c) c)) 
                                    #:entity-handler (entity-handler (lambda (e h) e))  
                                    field ...)
              (->* anys 
                   [#:handler (-> name? name?) 
                    #:entity-handler (specific-entity-handler? name?)]
                    name?)
               (name #f handler entity-handler field ...))))]))



(define (component-handler->game-handler ch)
  (lambda (g e c)
    (update-component e c ch)))

(define (entity-handler->game-handler eh)
  (lambda (g e c)
    (eh e c)))


(define (id-generator start)
  (define TEMP 0)
  (lambda ()
    (set! TEMP (add1 TEMP))
    TEMP))

(define/contract (tick g)
  (-> game? game?)
  (game (map (curry tick-entity g) 
             (game-entities g))))

(define/contract (tick-entity g e)
  (-> game? entity? entity?)

  
  (define temp-entity (struct-copy entity e))
  (for ([c (entity-components e)])

    (define lifted-component-handler 
      (component-handler->game-handler
        (component-handler c)))

    (define lifted-entity-handler 
      (entity-handler->game-handler
        (component-entity-handler c)))


    (set! temp-entity (lifted-component-handler g e c))
    (set! temp-entity (lifted-entity-handler g temp-entity c)))

  (entity (entity-id e) ;Component functions can't change an entity's id.
          (entity-components temp-entity)))






(define/contract (has-id? e)
  (-> entity? boolean?)
  (number? (entity-id e)))

(define (all-entities pred?)
  (lambda (g)
    (andmap pred? (game-entities g))))


(define (init-ids g)
  (-> game? game?)
  (game (map set-id! (game-entities g))))


(define next-entity-id (id-generator 0))

(define (set-id! e)
  (-> entity? entity?)
  (struct-copy entity e
               [id (next-entity-id)]))

(define (get-component e pred?)
  (findf pred? (entity-components e)))

(define/contract (component=? c1 c2)
  (-> component? component? boolean?)
  (eq? (component-id c1)
       (component-id c2)))

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

;TODO: remove-component

(define/contract (add-component e c)
   (-> entity? component? entity?)
   (struct-copy entity e
                [components (append (entity-components e) 
                                    (list c))])) 

(define/contract (remove-component e c)
   (-> entity? (or/c procedure? component?) entity?)

   (define p (if (component? c)
                 (curry component=? c)
                 c))

   (struct-copy entity e
                [components (filter-not p (entity-components e))])) 


