#lang racket

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
         init-ids
         all-entities
         has-id?
         tick
         tick-entity
         )


(require (for-syntax racket))
(require (for-syntax racket/syntax))

(struct component (id handler)  #:transparent)
(struct entity (id components)  #:transparent)
(struct game (entities) #:transparent)

(define/contract (new-game . es)
  (->* () #:rest (listof entity?) game?)
  (game es)) 
(define/contract (new-entity . cs)
  (->* () #:rest (listof component?) entity?)
  (entity #f cs))

(define-syntax (define-component stx)
  (syntax-case stx ()
    [(_ name (field ...))
     (with-syntax [ (other-constructor (format-id #'name "make-~a" #'name)) ] 
        #`(begin
             (struct name component (field ...) #:transparent
               #:constructor-name other-constructor
               #:omit-define-syntaxes)

             (define (name #:handler (handler (lambda (g e c) c)) field ...)
               (other-constructor #f handler field ...)) 

           ))]))


(define (component-handler->entity-handler f)
  (lambda (g e c)
    (update-component e c f)))

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

    (define handler 
      (component-handler->entity-handler (component-handler c)))

    (set! temp-entity (handler g e c)))

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

(define/contract (update-component e old-c f)
  (-> entity? component? procedure? entity?)
  (define cs (entity-components e))
  (define i  (index-of cs old-c ;TODO: Ooooo need to use component=? here!  Check by ids!!  
                       ;TODO: Shit.  components shouldn't be able to change their own ids
                       
                       ))


  (struct-copy entity e
               [components (list-set cs i (f old-c))]))

;TODO: add-component, remove-component, get-component
;  Sprite management is the big one...
;  Big steps: Get rendering working
;             Get physics working

;TODO: side quest.  Make sound toggleable...

