#lang racket

(provide time-manager-entity
         reached-game-count?
         stop-game-counter)

(require "../game-entities.rkt"
         "../components/counter.rkt"
         "../components/every-tick.rkt"
         2htdp/image
         posn)

(define (time-manager-entity #:components c . cs)
  (define components (flatten (cons c cs)))
  
  (sprite->entity empty-image
                  #:name "time manager"
                  #:position (posn 0 0)
                  #:components (counter 0)
                               (every-tick (change-counter-by 1))
                               components))

(define (reached-game-count? num)
  (lambda (g e)
    (define game-count (get-counter (get-entity "time manager" g)))
    (= game-count num)))

(define (stop-game-counter)
  (lambda (g e)
    (remove-component e every-tick?)))