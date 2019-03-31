#lang racket

(provide (rename-out [make-spawner spawner])
         spawn
         spawner?
         )

(require "./define-component.rkt"
         "./base.rkt"
         "./crud.rkt"
         "./runtime.rkt")

(define-component spawner (entities))

(define (make-spawner)
  (new-entity
    (new-spawner '())))

;A game function for use with #:game-handler.
;Can be placed on any entity.  Finds the spawner.  Seems to support the usual usecase: "Hey put a spawner in your game, the start attaching this game-handler to whatever components make sense in your app.
(define/contract (spawn maybe-child)
   (-> (or/c entity? procedure?) 
       (-> game? entity? component? game?))              


   (define/contract (f g parent c)
     (-> game? entity? (or/c component? procedure?) game?)
     (define child (if (procedure? maybe-child)
                       (maybe-child)
                       maybe-child))

     (define with-id 
       (struct-copy entity child
                    [id (next-entity-id)]))


     (define ret (update-entity g 
                                (has-component spawner?) 
                                ;TODO: Should we be able to use entity handlers in update-entity?
                                (lambda (e)
                                  (update-component e spawner? (update-spawner-entities
                                                                 (curry cons with-id))))))
     
     ret)
   
   f)
