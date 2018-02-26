#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (struct-out key-movement))

(struct key-movement (speed))

(define (update-key-movement g e c)
 (update-entity e posn?
                 (curry posn-add
                        (velocity-from-buttons (game-input g)
                                               (key-movement-speed c)))))

(define/contract (velocity-from-buttons btn-states speed)
  (-> button-states? number? posn?)
  (define leftVel  (if (button-states-left btn-states) (- speed) 0))
  (define rightVel (if (button-states-right btn-states)   speed  0))
  (define upVel    (if (button-states-up btn-states) (- speed) 0))
  (define downVel  (if (button-states-down btn-states)   speed  0))
  (posn (+ leftVel rightVel)
        (+ upVel downVel)))

(new-component key-movement?
               update-key-movement) 