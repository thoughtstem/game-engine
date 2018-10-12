#lang racket

(provide producer-of) 

(require "../game-entities.rkt"
         "../entity-helpers/carry-util.rkt"
         "../entity-helpers/sprite-util.rkt"
         "./lock-to.rkt"
         "./observe-change.rkt"
         "./backdrop.rkt"
         "./on-key.rkt"
         "../component-util.rkt"
         posn
         threading)

(define (producer-of to-carry)
  (define to-clone
    (~> to-carry
        (update-entity _ posn? (posn 0 0))
        (add-components _
         (movable #:carry-offset (posn 20 0))
         (lock-to "player" #:offset (posn 20 0))
         (observe-change carried? 
                         (λ(g e1 e2)
                           (if (carried? g e2)
                               (begin
                                 (remove-component e2 active-on-bg?))
                               (begin
                                 (add-component e2 (active-on-bg (game->current-tile g)))
                                 ))  ))  )
        (remove-component _ physical-collider?)  ))
  
  (list
   (on-key 'z
           #:rule (and/r near-player?
                         nearest-to-player? 
                         (not/r (other-entity-locked-to? "player")))
           (do-many (spawn to-clone)))))



