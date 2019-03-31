#lang racket

;TODO: Do all entities and components always have unique ids?  There are probably bugs here.  More tests.

;TODO: Need to figure out the handler combinators.

;TODO: Need to document the big four CRUDs: update-component, get-component, add-component, and remove-component.   Especially the predicate behaviour (remove all? remove first?) (update all?  update first?).  Are we overloading the function too much?  Should we break the functionality out into different functions?
  ; Also all the nice stuff that define-component gives you

;TODO: 
;  Sprite management is the big one...
;  Big steps: Get rendering working
;             Get physics working

;TODO: side quest.  Make sound toggleable...

(provide (except-out (all-from-out "./base.rkt")
                     game entity)

         (rename-out [new-game game]
                     [new-entity entity]) 

         (all-from-out "./define-component.rkt") 
         (all-from-out "./crud.rkt") 

         (all-from-out "./runtime.rkt")
         (all-from-out "./printer.rkt")
         (all-from-out "./spawns.rkt"))


(require "./define-component.rkt"
         "./handler-util.rkt"
         "./base.rkt"
         "./crud.rkt"
         "./runtime.rkt"
         "./printer.rkt"
         "./spawns.rkt")
