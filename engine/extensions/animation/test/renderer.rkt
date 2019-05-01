#lang racket

(require "../animated-sprite.rkt"
         "../renderer.rkt"
         "../../meta-components.rkt")

(require "../../../core/main.rkt")

(require "../../../core/test/conway.rkt"
         threading
         (prefix-in h: 2htdp/image))


;TODO: Wrap up this project of figuring out what to do with meta components.  
;  Use these existing rendering tests to prototype a replacement.
;  Delete the meta-components directory, or make them into macros or something.

;  - Look at the red/green pooper example.  There must be ways to simplify that.
;    What is the abstraction?  What is the language?  Shall we pivot the model once again?
;      If we are pivoting, can we pivot gradually?  A pivot plan, so to speak?
;    If we didn't pivot all at once, what would we do instead with that time?



;TODO: CLean up this doc. It has too much brainstorming in it.

;TODO: Can we simplify the component model so that components only update themselves?  
; If so, that would be huge.
;
;for-ticks and stuff like that can be specialized handler functions.
;
;Just note that copying a component to another entity means copying along all of its update behaviour -- which is exactly what you want...  Just gets weird when it's a (position ...) and you don't expect it to have a behaviour attached.  But that's mostly just a mental pivot for me.  Doc the new behaviour.  Update the docs.


;TODO: COnsider a #:render on components.  Could this be a better/other paradigmn for rendering games.  Compare with (play g) triggering a sprite scrape on every tick.
;  Makes it weirdly like react...


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

    #;
    (after-ticks 50 (die))))

;TODO: Let's make this less gross...
(bullet 'green)
(bullet 'red)
(bullet 'blue)


(define-component weapon  (bullet))
(define-component counter (n))
(define-component shooter ())


(define g
  (game
    (entity

      (position 200 200
                #:update (update:position/x^ (curry + 7)))
        
      (sprite (h:circle 20 'solid 'red))

      (counter 0 
               #:update (update:counter/n^ add1))

      (weapon (bullet 'green)
              #:update 
              (lambda (g e c)
                (update-component e weapon?
                                  (update:weapon/bullet c
                                    (bullet (if (odd? (get:counter/n e)) 
                                              'red 
                                              'green)))))) 
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









