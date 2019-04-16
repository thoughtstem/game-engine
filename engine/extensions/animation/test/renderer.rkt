#lang racket

(require "../animated-sprite.rkt"
         "../renderer.rkt"
         "../../meta-components.rkt")

(require "../../../core/main.rkt")

(require "../../../core/test/conway.rkt"
         threading
         (prefix-in h: 2htdp/image))

;TODO: Figure out how to do tests
;How to test renderer?  (Screenshots??)

;TODO: Being able to slow down games will help with testing.

;TODO: Rendering two games at once.  A child game?  Waahhh..

;TODO: Start doing benchmarking early, so we know what to expect later.  How big of a conway implementation can we get?
;      Write tests for speed (figure out how to make implementation dependent)

;TODO: Start documenting the renderer so we can figure out what its features need to be.  Don't just start implementing stuff willy nilly. 

;TODO:
;What about sprites being constructed at runtime.  Just don't do it?  Throw a warning...  Maybe allow Document the behaviour.

;TODO: 
;User input needs to get handled at the lux level.  Do we try to separate it from the renderer?

(define (bullet c) 
  (entity 
    (position 200 200)
    (sprite (h:circle 5 'solid c))
    (after-ticks 100 (die))))

#;
(lux-start (game
             (entity
               (position 200 200)
               (sprite (h:circle 20 'solid 'red))
               (new-component #:update
                              (update:position/x^ (curry + 5)))
               (forever
                      (sequence
                        (for-ticks 5
                                   (spawn-here (bullet 'green)))
                        (for-ticks 5
                                   (spawn-here (bullet 'blue))))))

             (entity
               (position 200 200)
               (sprite (h:circle 20 'solid 'orange))
               (new-component #:update
                              (update:position/y^ add1)))))




;make a way to patch/expand?  What's a good language for making conway games??   (Composing other games together??)
#;
(define g0 (~> (conway-game 5) 
               (conway-game-set _ 1 1 #t)
               (conway-game-set _ 2 1 #t)
               (conway-game-set _ 3 1 #t)
               (conway-game-set _ 1 2 #t)
               (conway-game-set _ 2 2 #t)
               (conway-game-set _ 3 2 #t)
               (conway-game-set _ 1 3 #t)
               (conway-game-set _ 2 3 #t)
               (conway-game-set _ 3 3 #t)))


(define dead-sprite
  (sprite (h:circle 5 'solid 'black)))

(define live-sprite
  (sprite (h:circle 5 'solid 'green)))

(define (live/dead-sprite-swap g e c)
  (if (entity-conway-alive? e) ;TODO: Consider whether the constant polling can be avoided... on-change?  Something better?
    (update-component e sprite? live-sprite)
    (update-component e sprite? dead-sprite)))

(define (augment g)
  (define (aug-e e)
    (define c (get-component e conway?))  

    ;TODO: make add-components
    (add-component
      (add-component
        (add-component e 
                       (position
                         (+ 50 (* 10 (conway-x c)))
                         (+ 100 (* 10 (conway-y c)))))
        dead-sprite)
      (new-component #:update live/dead-sprite-swap)) 
    )

  (apply game (map aug-e (game-entities g))))


;So the algorithm for looking up neighbors SUCKS.  Can game-engine provide a fast but clean way of searching or some way of storing references?

;Conway doesn't scale well at all.
;  (Though, it's worth noting that this is a LOT of entities for a typical game.  Before we invest too much into optimizations, let's consider if this is maybe just a poor way to do conway's game of life in game-engine, or that conway's game of life is a poor representation of what you'd want to do in a typical game.
;
;  Maybe you'd have one conway singleton component that ticks the underlying list data structure.  Then you'd have other entities watching that structure and spawning, dying...  Way fewer entities that way.  And the lookup problem is solved...
;  )

(define g0
  (overlay
    (conway-game '((* * *)
                   (* _ *)
                   (* * *)))
    
    (conway-game 11)))

(define g1
  (beside g0 g0))

(define g2
  (beside g1 g1))

;TODO: Make a way to slow this down at the game level, not just at the rendering level....
(play (augment g2))





