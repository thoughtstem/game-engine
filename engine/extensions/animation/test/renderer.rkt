#lang racket

(require "../animated-sprite.rkt"
         "../renderer.rkt"
         "../../meta-components.rkt")

(require "../../../core/main.rkt")

(require "../../../core/test/conway.rkt"
         threading
         (prefix-in h: 2htdp/image))

;Weird bug where a Position component seems to be getting into the game list?  At least that's how I read the error:
Error ticking entity
  Entity: 19 
    COMPONENT: Position, 11
      (lambda (p)  (posn-add p (posn 0 1)))
      #s(posn 200 201)
      #<procedure:...ne-component.rkt:245:47>
    COMPONENT: sprite, 12
      unknown-update-function
      sprite--2994216625177786298
      #f
    COMPONENT: Counter, 13
      add1
      1
      #<procedure:...ne-component.rkt:245:47>
    COMPONENT: Weapon, 14
      (lambda (b e)  (if (odd? (get-Counter e))  (bullet (quote red))  (bullet (quote green))))
      #(struct:entity 22 (#(component Position 20 #<procedure:...ne-component.rkt:245:47> identity #s (posn -1 -1)) # (component sprite 21 #f unknown-update-function sprite-3833518649628565063)) #f)
      #<procedure:...ne-component.rkt:245:47>
    COMPONENT: Shooter, 18
      (lambda (v e)  (define current-bullet (get-Weapon e))  (spawn-me (move-to (get-Position e) current-bullet)))
      #<void>
      #<procedure:...ne-component.rkt:245:47>

entity-components: contract violation
  expected: entity?
  given: '#(component Position 20 #<procedure:...ne-component.rkt:245:47> identity #s (posn -1 -1))
  context...:
   /usr/share/racket/collects/racket/private/more-scheme.rkt:163:2: select-handler/no-breaks
   /home/thoughtstem/Desktop/Dev/game-engine/engine/core/runtime.rkt:58:4: for-loop
   /home/thoughtstem/Desktop/Dev/game-engine/engine/core/runtime.rkt:55:2: for-loop
   /home/thoughtstem/Desktop/Dev/game-engine/engine/core/runtime.rkt:54:0: tick-entities
   /home/thoughtstem/Desktop/Dev/game-engine/engine/core/runtime.rkt:35:0: tick
   /home/thoughtstem/Desktop/Dev/game-engine/engine/extensions/animation/renderer.rkt:41:3: word-tick
   /home/thoughtstem/.racket/7.0/pkgs/lux/word.rkt:142:18
   /home/thoughtstem/.racket/7.0/pkgs/lux/word.rkt:84:0: call-with-chaos
   "/home/thoughtstem/Desktop/Dev/game-engine/engine/extensions/animation/test/renderer.rkt":  [running body]
   for-loop
   run-module-instance!125
   perform-require!78


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




;TODO: Let's make this less gross...


(define-component weapon  (bullet))
(define-component counter (n))
(define-component shooter ())

(require posn)
(define-signal Counter number?)
(define-signal Position posn?)
(define-signal Weapon  entity?)
(define-signal Shooter boolean?)


(define (bullet c) 
  (entity 
    (Position (posn -1 -1) identity)
    (sprite (h:circle 5 'solid c))

    #;
    (after-ticks 50 (die))))

(bullet 'green)
(bullet 'red)
(bullet 'blue)

(define g
  (game
    (entity

      (Position (posn 200 200) 
                (lambda (p) 
                  (posn-add p 
                            (posn 0 1))))
        
      (sprite (h:circle 20 'solid 'red))


      (Counter 0 add1)

      (Weapon (bullet 'green) 
              (lambda (b e)
                (if (odd? (get-Counter e))
                  (bullet 'red)    
                  (bullet 'green))))


      (Shooter (void) 
               (lambda (v e)
                 (define current-bullet (get-Weapon e)) 
                 (spawn-me 
                   (move-to (get-Position e) current-bullet))
                 ))

      #;
      (counter 0 #:update (update:counter/n^ add1))

      #;
      (weapon (bullet 'green)
              #:update 
              (lambda (g e c)
                (update-component e weapon?
                                  (update:weapon/bullet c
                                    (bullet (if (odd? (get:counter/n e)) 
                                              'red 
                                              'green)))))) 
      #;
      (shooter
        #:update (compose-handlers (for-ticks 20)
                                   (lambda (g e c)
                                     (define current-bullet (get:weapon/bullet e))
                                     (add-component e 
                                                    (spawner (move-to-parent e current-bullet))))))

      #;
      (for-ticks 200
                 (spawn-here (bullet 'green)))

      #;
      ((
                   (spawn-here (bullet 'green)))
        (for-ticks 5
                   (spawn-here (bullet 'blue))))
      #;
      (forever
        (sequence
          (for-ticks 5
                     (spawn-here (bullet 'green)))
          (for-ticks 5
                     (spawn-here (bullet 'blue)))))
      
      )

    #;
    (entity
      (name "orange-dude")
      (position 200 200)
      (sprite (h:circle 20 'solid 'orange))
      (new-component #:update
                     (update:position/y^ add1))
      
      )))


;Can we make this kind of query easier to make?
;  "How does e with name ___'s ___ vary over the next ___ ticks?"
#;
(map 
  (compose y 
           (curryr get-by-name "orange-dude"))
  (tick-list g 5))

;TODO: Bug in the sprite cache.  Disabled for now.  But need to fix. 

;TODO: Bugginess with forever, sequence, for-ticks
;       Could keep tracking down the specific bugs, but what's really going on is that it's fucking hard to reason about these meta components.  It was hard before mutability.  Now it's impossible.  Go back to the drawing board on these.  Why do we need them?  Is there some other abstraction that would be better?  

(play! g)        
#;
(debug
  (tick! g)
  (tick! g)
  (tick! g)
  (tick! g)
  (tick! g))




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




)




