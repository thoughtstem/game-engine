#lang racket

(provide get-movement-system
         (rename-out [make-movement-system
                      movement-system]) )

(require "../../core/main.rkt"
         "../common-components/main.rkt")

(define-component movement-system entity?)

(define (make-movement-system 
          #:direction-update (direction-update identity))
  (list 
    (movement-system
      (entity
        (direction (posn 0 0) 
                   (^ direction-update))
        (position (posn 200 200)
                  (posn-add 
                    (get-position)
                    (get-direction))))
      (^ tick-entity)) ))

