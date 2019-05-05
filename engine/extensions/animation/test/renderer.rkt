#lang racket

(require "../animated-sprite.rkt"
         "../renderer.rkt")

(require "../../../core/main.rkt")

(require "../../../core/test/conway.rkt"
         threading
         (prefix-in h: 2htdp/image))


;Make sure the mutable/non-mutable games both work the same (with fps only difference)


;Keep having ideas about using rosette or constraint based programming to do
; * Procedural geneartion
; * Generating entire games, sequences
; * Flockin behavior
;Or other meta stuff:
; * A component containing a game and a behaviour that ticks it...
; * Run subgames within a game...
; * Procedurally create games at runtime, run them, do something with the result.
; * "Bake" a game, by running it and observing its values.  Faster now as a sub-game...


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
(define-component Position posn?)
(define-component Counter number?)

(begin

  (define dead-sprite
    (register-sprite
      (h:circle 5 'solid 'red))) 

  (define live-sprite
    (register-sprite
      (h:circle 5 'solid 'green))) 

  (define g 
    (game
      (entity 
        (Position (posn 200 200))
        (Counter 0 (+ 1 (get-Counter)))

        #;
        (Sprite live-sprite
                (if (odd? (get-Counter))
                  live-sprite
                  dead-sprite)))))  


  (play g) 
  )

#;
(begin

  (define-component Weapon  entity?)
  (define-component Shooter boolean?)
  (define-component Killer boolean?)

  (define (bullet c) 
    (entity 
      (Position (posn -1 -1) 
                (let ()
                  (posn-add 
                    (posn (random -1 2)
                          (random -1 2))
                    (get-Position)
                    )))
      (Sprite (register-sprite (h:circle 5 'solid c)) 
              (get-Sprite))
      (Counter 0 
               (+ 1 (get-Counter)))
      (Killer  #f 
               (if (= 50 (get-Counter))
                 (despawn)
                 #f))))

  (bullet 'green)
  (bullet 'red)
  (bullet 'blue)

  (define (move-down)
    (posn-add (get-Position)
              (posn 0 5)))

  (define (move-up)
    (posn-add (get-Position)
              (posn 0 -5)))

  (define g
    (game
      (entity
        (Counter 0 (+ (get-Counter) 1))

        (Position (posn 200 200) 
                  (if (odd? (floor 
                              (/ (get-Counter)
                                 50)))
                    (move-up)
                    (move-down)))

        (Sprite (register-sprite (h:circle 20 'solid 'red))
                (get-Sprite))



        (Weapon (bullet 'green) 
                (if (odd? (get-Counter))
                  (bullet 'red)    
                  (bullet 'green)))


        #;
        (Shooter #f
                 (let 
                   ([current-bullet (get-Weapon)])

                   (spawn 
                     (move-to (get-Position) current-bullet)))))))


  #;
  (play! g)

  ;TODO: Still works?
  (play g)

  )



#;
(begin

  (define dead-sprite
    (register-sprite
      (h:circle 5 'solid 'red))) 

  (define live-sprite
    (register-sprite
      (h:circle 5 'solid 'green))) 

  (define (augment g)
    (define (aug-e e)
      (define c (get-component e conway?))

      ;TODO: make add-components
      (if c

        (add-components e
                        (Position
                          (posn
                            (+ 50 (* 10 (conway-x (get-conway c))))
                            (+ 100 (* 10 (conway-y (get-conway c))))))
                        (Sprite dead-sprite
                                (if (conway-alive? (get-conway))
                                  live-sprite 
                                  dead-sprite)))
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
      (conway-game quilted-donut)))

  #;
  (debug-tick to-play)

  #;
  (play to-play)


  (play! to-play)  




  )




