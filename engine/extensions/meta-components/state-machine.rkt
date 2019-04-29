#lang racket

#;
(provide (rename-out [ default-state-machine state-machine]))

(require "../../core/main.rkt")

;A state machine is one of the primitive kinds of meta-components.  When attached to an entity, a state-machine component gives the entity a collection of behaviours (a state, which is also a component) 

;Why not just have a branch?  It's one component now, another component otherwise.  Based on a rule -- which is a handler-like thing but can return false.  Or should it be a component that returns a falsey-component.

(define-component state-machine (states transitions))


(define (update-state-machine g e c)
  e
  
  )


(define (default-state-machine state-components  transition-??s)
  (state-machine state-components
                 transition-??s
                 #:update update-state-machine))
