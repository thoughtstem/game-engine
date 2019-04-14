#lang racket

(provide spawner
         spawn 
         update:spawner/to-spawn
         update:spawner/to-spawn^
         spawner-to-spawn
         spawner?

         dead
         dead?
         die)

(require "./define-component.rkt"
         "./base.rkt"
         "./crud.rkt"
         )

(define-component spawner (to-spawn))
(define-component dead ())

(define (spawn to-spawn 
               (before-spawn (lambda (parent child) child)))

  (new-component
    #:update
    (lambda (g e c)
      (add-component e 
                     (spawner (before-spawn e to-spawn))))))


;This makes things like (after-ticks 5 (die))
; work.  But it feels redundant to have a component that adds the dead component.  Think through how after-ticks/for-ticks/etc are all working. ; Maybe if we clean them up, this can get clarified too.. 
;  Their semantics requires that their child components have update functions.  Maybe that makes sense, but just need to think about it and do a contract error if someone passes in a child wihtout an update
(define (die)
  (new-component #:update
                 (add-component^ (dead))))

