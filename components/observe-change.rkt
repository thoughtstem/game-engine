#lang racket

(provide (except-out (struct-out observe-change) observe-change)
         (rename-out [make-observe-change observe-change]))

(require "../game-entities.rkt"
         ;"../component-util.rkt"
         posn)

(struct observe-change (rule last-val previous-entity on-change))



(define (make-observe-change rule on-change)
  (observe-change rule (void) (void) on-change))


#;(observe-change carried?
                  (λ(g e)
                    (if (carried? e)
                        (displayln "Picked up")
                        (displayln "Dropped"))
                    e))

(define (update-observe-change g e c)
  (define current-val ((observe-change-rule c) g e))
  
  (define last-val (observe-change-last-val c))

  (define new-c (struct-copy observe-change c
                             [last-val current-val]
                             [previous-entity e]))

  (define new-e (update-entity e (λ(x) (eq? x c)) new-c))
  (define prev-e (observe-change-previous-entity c))

  (if (eq? current-val last-val)
      new-e
      ((observe-change-on-change c) g prev-e new-e)))

(new-component observe-change?
               update-observe-change)


