#lang racket

;TODO: Doc this component

(provide spawn-manager 
         spawn-manager?
         spawn
         spawner?
         
         spawn-queue-empty?)

(require game-engine/engine/core)

(define-component spawner (spawn-queue))

(define (spawn-manager)
  (new-entity
    (new-spawner 
      ;Start with an empty spawn queue 
      '() 
      ;Flush the spawn queue on every tick...
      #:game-handler (do-many 
                       do-spawns
                       clear-spawner))))

(define spawn-manager? (has-component spawner?))


(define (do-spawns g e c)
  (define to-spawn (spawner-spawn-queue c)) 
  
  (foldl (reverse-params add-entity)  ;TODO: Doc reverse params...
         g to-spawn))

(define (clear-spawner c)
  (set-spawner-spawn-queue c '()))

(define (get-spawn-manager g)
  (get-entity g spawn-manager?))

(define (get-spawner g)
  (get-component (get-spawn-manager g)
                 spawner?))

(define (get-spawn-queue g)
  (spawner-spawn-queue (get-spawner g)))

(define (spawn-queue-empty? g)
  (empty? (get-spawn-queue g)))

;A game function for use with #:game-handler.
;Can be placed on any entity.  Finds the spawner.  Seems to support the usual usecase: "Hey put a spawner in your game, the start attaching this game-handler to whatever components make sense in your app.
(define/contract (spawn maybe-child)
   (-> (or/c entity? procedure?) 
       (-> game? entity? component? game?))              

   (define/contract (f g parent c)
     (-> game? entity? component? game?)
     (define child (if (procedure? maybe-child)
                       (maybe-child)
                       maybe-child))

     (update-entity g 
                    (has-component spawner?) 
                    (update-entity-first-spawner
                      (update-spawner-spawn-queue
                        (curry cons child))))) 
   f)
