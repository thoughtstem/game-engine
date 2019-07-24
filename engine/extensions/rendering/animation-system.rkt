#lang racket 
(provide (rename-out [make-animation-system
                      animation-system]) 
         get-animation-system)

(require "../../core/main.rkt"
         "../input/main.rkt"
         "./animated-sprite.rkt"
         "../common-components/main.rkt")

(define-component animation-system entity?)

;Cool.  What's the next abstraction step?
(define (make-animation-system 
          #:counter-update (counter-update add1)
          #:direction-update (direction-update 
                               identity)
          up-frames right-frames down-frames left-frames)
  (list
    (animation-system
      (entity 
        (counter 0 (^ counter-update))
        (direction (posn 0 0) (^ direction-update))

        (facing 'left
                (cond 
                  [(> 0 (posn-x (get-direction)))
                   'left ]
                  [(< 0 (posn-x (get-direction)))
                   'right]
                  [(> 0 (posn-y (get-direction)))
                   'up ]
                  [(< 0 (posn-y (get-direction)))
                   'down]
                  [else (get-facing)]))

        (sprite (first left-frames) 
                (list-ref 
                  (cond 
                    [(eq? 'up (get-facing)) up-frames]
                    [(eq? 'right (get-facing)) right-frames]
                    [(eq? 'left (get-facing)) left-frames]
                    [(eq? 'down (get-facing)) down-frames])
                  (if (equal? (posn 0 0) (get-direction))
                    0  
                    (remainder (get-counter) 
                               (length left-frames))))))   
      (^ tick-entity)) 

    (sprite (first left-frames)
            (get-sprite (get-animation-system)))))


