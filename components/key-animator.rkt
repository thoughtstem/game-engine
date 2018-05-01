#lang racket

(require "../game-entities.rkt")
(require "./animated-sprite.rkt")
(require posn)
(require threading)


(provide key-animator)

(struct key-animator (current animation))

(define (update-key-animator g e c)
  (define pdir (velocity-from-buttons (game-input g)
                                     5))
  (define new-dir (cond
                    [(= 0 (posn-x pdir) (posn-y pdir)) 'none]
                    [(> (posn-x pdir) 0) 'right]
                    [(< (posn-x pdir) 0) 'left]
                    [(< (posn-y pdir) 0) 'up]
                    [(> (posn-y pdir) 0) 'down]))

  (define current-dir (key-animator-current c))
  (if (equal? new-dir current-dir)
      e
      (~> e
          (update-entity _ key-animator?
                         (key-animator new-dir (key-animator-animation c)))
          (update-entity _ animated-sprite?
                         ((key-animator-animation c) new-dir)))))



(define/contract (velocity-from-buttons btn-states speed)
  (-> hash? number? posn?)
  (define leftVel  (if (button-down? 'left btn-states) (- speed) 0))
  (define rightVel (if (button-down? 'right btn-states)   speed  0))
  (define upVel    (if (button-down? 'up btn-states) (- speed) 0))
  (define downVel  (if (button-down? 'down btn-states)   speed  0))
  (posn (+ leftVel rightVel)
        (+ upVel downVel)))


(new-component key-animator?
               update-key-animator)


