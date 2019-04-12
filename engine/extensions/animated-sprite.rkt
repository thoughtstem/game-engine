#lang racket

(require "../core/main.rkt")

;Can we abstract out things like delay and the counter?
;  Seems like that kind of thing comes up a lot...
;  It could be implemented as a game... an incrementing counter with a max, loops back...
(define-component animated-sprite (frames current-frame delay))

(define (next-frame as)
  ;Pitty this doesn't work... :(  Maybe we can get it to?  Or make something like it.
  ;(match-define (animated-sprite frames current-frame delay) as)

  (define frames (animated-sprite-frames as))
  (define current-frame (animated-sprite-current-frame as))
  (define delay (animated-sprite-delay as))

  ;Ugh, should we just cave and make them structs...?
  #;
  (struct-copy animated-sprite (frames (add1 current-frame) delay))
  
  (update:animated-sprite/current-frame as (add1 current-frame))
  )

(define update-animated-sprite
  (update-component^ next-frame))


(begin ;module+ test
  (require "./debug-manager.rkt")

  ;TODO: THe message when you forget to pass in the right number of 
  ;initialized arguments to a component is confusing as hell
  (define e (entity (animated-sprite '(a b c) 0 5 
                                     #:update update-animated-sprite)))  
  (debug-ticks 4 (game e)))
