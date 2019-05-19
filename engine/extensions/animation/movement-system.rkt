#lang racket

(provide get-movement-system
         (rename-out [make-movement-system
                      movement-system]) )

(require "../../core/main.rkt"
         "./animated-sprite.rkt"
         "./common-components.rkt")

;TODO: Doc
; Add this component, then use it to control your position:
; (position (posn 0 0) 
;              (get-position (get-movement-system)) 

(define-component movement-system entity?)

(define (make-movement-system 
          #:direction-update (direction-update identity))
  (list 
    (movement-system
      (entity
        (direction #f (^ direction-update))
        (position (posn 200 200)
                  (posn-add 
                    (get-position)
                    (get-direction))))
      (^ tick-entity))
     
    ))

