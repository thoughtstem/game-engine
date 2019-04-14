#lang racket

(provide spawner
         spawn 
         update:spawner/to-spawn
         update:spawner/to-spawn^
         spawner-to-spawn
         spawner?
         dead
         dead?)

(require "./define-component.rkt"
         "./base.rkt"
         "./crud.rkt"
         )

(define-component spawner (to-spawn))
(define-component dead ())

(define (spawn to-spawn)
  (new-component
    #:update
    (lambda (g e c)
      (add-component e 
                     (spawner to-spawn)))))

