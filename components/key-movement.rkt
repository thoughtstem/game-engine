#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (struct-out key-movement)
         change-speed-by
         get-speed)

(struct key-movement (speed))

(define (update-key-movement g e c)
 (update-entity e posn?
                 (curry posn-add
                        (velocity-from-buttons  g
                                               (key-movement-speed c)))))

(define/contract (velocity-from-buttons game speed)
  (-> game? number? posn?)
  (define leftVel  (if (button-down? 'left game) (- speed) 0))
  (define rightVel (if (button-down? 'right game)   speed  0))
  (define upVel    (if (button-down? 'up game) (- speed) 0))
  (define downVel  (if (button-down? 'down game)   speed  0))
  (posn (+ leftVel rightVel)
        (+ upVel downVel)))

;Not clear either...  Move or simplify with better API
(define (change-speed-by n)
  (lambda (g e)
    (define increase (lambda (k)
                       (key-movement (+ (key-movement-speed k) n))))
    (update-entity e key-movement? increase)))

(define (get-speed e)
  (key-movement-speed (get-component e key-movement?)))

(new-component key-movement?
               update-key-movement) 