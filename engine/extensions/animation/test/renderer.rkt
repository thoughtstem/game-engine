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

(define dead-sprite
  (sprite (h:circle 5 'solid 'red)))

(define live-sprite
  (sprite (h:circle 5 'solid 'green)))

(define (entity-conway-alive? e)
  (conway-alive?
    (first (entity-components e))))

(define (live/dead-sprite-swap g e c)
  (if (entity-conway-alive? e) 
    (update-component e 2 live-sprite)
    (update-component e 2 dead-sprite)))

(define (augment g)
  (define (aug-e e)
    (define c (get-component e conway?))  

    ;TODO: make add-components
    (if c
      (add-component
        (add-component
          (add-component e 
                         (position
                           (+ 50 (* 10 (conway-x c)))
                           (+ 100 (* 10 (conway-y c)))))
          dead-sprite)
        (new-component #:update 
                       live/dead-sprite-swap))
      e))

  (apply game (map aug-e (game-entities g))))


(require "../../../core/test/conway-impl.rkt")

(define donut
  '((* * *)
    (* _ *)
    (* * *)))

(define square-3 (square 3))

(define padded-donut
  (overlay  
    (square 11)
    donut))

(define quilt
  (beside
    (above donut square-3)
    (above square-3 donut)))

(define padded-donut3
  (beside (beside padded-donut padded-donut) padded-donut))

(define quilted-donut
  (above quilt padded-donut3))

(define quilted-donut2
  (above quilted-donut quilted-donut))

(define to-play
  (augment 
    #;
    (conway-game donut)
    (conway-game quilted-donut)))

#;
(debug-tick to-play)

#; ;Why is this erroring?
(play to-play)

(mutable!
  (play to-play)) 


;Another optimization, medium trickiness level:
;  entity components and game entities should be vectors, not lists when mutable-state is on  
;  (Need to Experiment, though: Would make updates faster, but adds and removals slower, maybe...)

;What's next big picture wise?  Can prolly stop optimizing for a while.









