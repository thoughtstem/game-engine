#lang racket

(require "../animated-sprite.rkt"
         "../renderer.rkt"
         "../../meta-components.rkt")

(require "../../../core/main.rkt")

(require "../../../core/test/conway.rkt"
         threading
         (prefix-in h: 2htdp/image))


;Okay, syntax is looking great.  But it's getting as slow as a butt.  Let's try putting in some optimizations.
;  Or, if necessary, refactor the runtime...

;WHyyyy don't we get better error line numbers from macro-defined functions?
;  Nope.  It's not the macros.  It's the way we catch errors in runtime.  Need to rethrow that shit somehow...  See "Error ticking entity" handler...

;Game-oriented programming?
;A game-based programming "paradigmn"?

;TODO: Wow! Switching to signals seems to be working well.   Logic in the red/green Pooper example was a lot simpler this time.  Another pass?  Or maybe a few more examples to get the feel for crafting logic with signals.  I want to know what abstractions we'll need before we get too deep into refactoring things.
;  For now, can experiment with "signals" without removing components.  Eventually clean everything up and rename signals to components (or behaviours).

;TODO: Wrap up this project of figuring out what to do with meta components.  
;  Use these existing rendering tests to prototype a replacement.
;  Delete the meta-components directory, or make them into macros or something.

;  - Look at the red/green pooper example.  There must be ways to simplify that.
;    What is the abstraction?  What is the language?  Shall we pivot the model once again?
;      If we are pivoting, can we pivot gradually?  A pivot plan, so to speak?
;    If we didn't pivot all at once, what would we do instead with that time?



;Finalize the rendering system.  Docs, and tests.
;   So we can move on to input...

;TODO: A few bugs leftover from refactoring for speed.
;   Making assuptions in renderer and animated-sprite about sprite? and position? components being at a known index.  We need to find a generalized abstraction for that.

;TODO: Start documenting the renderer so we can figure out what its features need to be.  Don't just start implementing stuff willy nilly. 

;TODO: Figure out how to do tests
;How to test renderer?  (Screenshots??)

;TODO: Rendering two games at once.  A child game?  Waahhh..

(require posn)
(define-signal Counter number?)
(define-signal Position posn?)
(define-signal Weapon  entity?)
(define-signal Shooter boolean?)
(define-signal Killer boolean?)

(define (bullet c) 
  (entity 
    (Position (posn -1 -1) 
              (get-Position))
    (Sprite (h:circle 5 'solid c))
    (Counter 0 
             (+ 1 (get-Counter)))
    (Killer  #f 
             (if (= 50 (get-Counter))
                    (despawn-me)
                    #f))))

(bullet 'green)
(bullet 'red)
(bullet 'blue)

(define g
  (game
    (entity

      (Position (posn 200 200) 
                (posn-add (get-Position)
                          (posn 0 1)))
        
      (Sprite (h:circle 20 'solid 'red))


      (Counter 0 (+ (get-Counter) 
                    1))

      (Weapon (bullet 'green) 
              (if (odd? (get-Counter))
                  (bullet 'red)    
                  (bullet 'green)))


      (Shooter #f
               (let 
                 ([current-bullet (get-Weapon)])

                 (spawn-me 
                   (move-to (get-Position) current-bullet)))))))

(play! g)


#;
(mutable!
  (debug-tick
    (debug-tick g)))

;Works but weirdly...
#;
(play g)



;All of the bullets getting spawned have the same id.  That's one problem.  Possibly because of htat, their ids seems suspicious.



;make a way to patch/expand?  What's a good language for making conway games??   (Composing other games together??)

#;
(

(define dead-sprite
  (Sprite (h:circle 5 'solid 'red)))

(define live-sprite
  (Sprite (h:circle 5 'solid 'green)))

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




)




