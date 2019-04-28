#lang racket

(require "../animated-sprite.rkt"
         "../renderer.rkt"
         "../../meta-components.rkt")

(require "../../../core/main.rkt")

(require "../../../core/test/conway.rkt"
         threading
         (prefix-in h: 2htdp/image))


;Finalize the rendering system.  Docs, and tests.
;   So we can move on to input...


;TODO: A few bugs leftover from refactoring for speed.
;   Making assuptions in renderer and animated-sprite about sprite? and position? components being at a known index.  We need to find a generalized abstraction for that.

;  Also the bullet test is failing.  not sure why.
;
;
;  The conway test is failing with mutability on, which should never happen.  It should be the same semantically, just faster...  Why?  Maybe easier to debug with contracts working again!



;TODO: Start documenting the renderer so we can figure out what its features need to be.  Don't just start implementing stuff willy nilly. 

;TODO: Figure out how to do tests
;How to test renderer?  (Screenshots??)

;TODO: Rendering two games at once.  A child game?  Waahhh..



(define (bullet c) 
  (entity 
    (position 200 200)
    (sprite (h:circle 5 'solid c))
    (after-ticks 100 (die))))

(define g
  (game
    (entity
      (position 200 200)
      (sprite (h:circle 20 'solid 'red))
      (new-component #:update
                     (update:position/x^ (curry + 1)))
      (for-ticks 100
                 (spawn-here (bullet 'green)))
      #;
      (sequence
        (for-ticks 5
                   (spawn-here (bullet 'green)))
        (for-ticks 5
                   (spawn-here (bullet 'blue))))
      #;
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

#;
(mutable!
  (debug-tick
    (debug-tick g)))

;Works but weirdly...
#;
(play g)

;Crashes for some reason...
(debug
  (tick! g)
  (tick! g)
  (tick! g)
  (tick! g))



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

#;
(play! to-play)  









