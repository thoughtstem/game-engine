#lang racket

(provide producer-of
         producer) 

(require "../game-entities.rkt"
         "../entity-helpers/carry-util.rkt"
         "../entity-helpers/sprite-util.rkt"
         "./lock-to.rkt"
         "./observe-change.rkt"
         "./on-start.rkt"
         "./backdrop.rkt"
         "./on-key.rkt"
         "../component-util.rkt"
         posn
         threading)


(define (display-entity e)
  (define i (draw-entity e))
  (define p (get-posn e))
  (define a (get-component e active-on-bg?))
  
  (displayln i)
  (displayln (~a "(posn "
                 (exact-round (posn-x p))
                 " "
                 (exact-round (posn-y p))
                 ")"))
  (displayln (~a "(active-on-bg "
                 (first (active-on-bg-bg-list a))
                 ")"))
  
  e)

(define (add-producer-of-self #:on-drop [on-drop display-entity])
  (lambda (g e)
    (add-components e
                    (producer-of e #:on-drop on-drop))))

(define (producer #:on-drop [on-drop display-entity])
   (on-start (add-producer-of-self #:on-drop on-drop)))

(define (producer-of to-carry #:on-drop (on-drop display-entity))
  (define to-clone
    (~> to-carry
        (update-entity _ posn? (posn 0 0))
        (add-components _
                        (movable #:carry-offset (posn 20 0) #:on-drop on-drop #:show-info? #t)
                        (lock-to "player" #:offset (posn 20 0)))
        (remove-component _ physical-collider?)  ))
  (list
   (on-key 'z
           #:rule (and/r near-player?
                         nearest-to-player? 
                         (not/r (other-entity-locked-to? "player")))
           (do-many (spawn to-clone)))))



