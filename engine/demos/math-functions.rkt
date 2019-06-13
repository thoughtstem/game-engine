#lang racket

(require game-engine
         2htdp/image
         threading )


;TODO: Clarify the intent here and make better examples
;  I guess the idea is to (re)create certain animation primitives.
;  I want to be able to operate at the level of "clips" -- being able to reverse, stretch, compress, append, and cut them together with some kind of nice little algebra.
;  Various classic easing effects would be really nice to show how to do

;TODO: Finally figure out delta time.  Be able to make FPS-stable animations.
;  See common-components/time/delta-time.rkt

;TODO: Draw paths with gridified physics entities in bg?  Cool visual.


(define red          (register-sprite (circle 10 'solid 'red)))
(define green-square (register-sprite (square 20 'solid 'green)))    


(define-syntax (start-as stx)
  (syntax-case stx ()
    [(_ start) start] 
    [(_ start (then-at ticks switch-to))
     (if ())
     ]
    [(_ start (then-at ticks switch-to) ...)
     (if ())
     ]
    )
  )

(define mr-ball
  (entity

    (counter 0 (^ add1))

    (position (posn 200 200)
              (posn 
                (start-as (posn-x (get-position))
                          (then-at 100 
                                   (add1 (posn-x (get-position))))
                          (then-at 150 
                                   (posn-x (get-position))))
                200))

    (sprite red
            (start-as (get-sprite)
                      (then-after 100
                                  green-square)))))



(define g (game mr-ball))

(play! g)


