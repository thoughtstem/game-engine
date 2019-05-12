#lang racket

(provide (rename-out [make-movement-system
                      movement-system]) )

(require "../../core/main.rkt"
         "./input.rkt"
         "./animated-sprite.rkt"
         "./common-components.rkt")

(define-component movement-system entity?)

(define (make-movement-system 
          #:direction-update (direction-update (thunk* (as-posn (get-current-input)))))
  (list 
    (movement-system
      (entity
        (direction #f (^ direction-update))
        (position (posn 200 200)
                  (posn-add 
                    (get-position)
                    (get-direction))))
      (^ tick-entity))
     
    (position (posn 0 0) 
              (get-position (get-movement-system)))))

