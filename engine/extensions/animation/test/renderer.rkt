#lang racket

(require "../animated-sprite.rkt"
         "../renderer.rkt"
         "../../meta-components.rkt")

(require "../../../core/main.rkt")

(require "../../../core/test/conway.rkt"
         threading
         (prefix-in h: 2htdp/image))



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


(define g
  (game
    (entity
      ;Essentially, an entity is a bunch of data with some initial state.
      ;And then that state evolves over time.
      ;The evolution is described as a collection of functions that take and return new versions of the entity on every tick.  These are folded together to create the next entity.

      ;
      ;But that seems like it could be overkill.  These functions really just need to snipe out certain values in the data and read/change them.
      ;  It also adds and removes components, but that could also be implemented with a queue -- a component (or meta structure) that stores a list of the components to be added, for example.

      ;One idea is to build a dataflow graph, like in FRP.
      ;  We would express the values as behaviours, etc.

      ;But that seems like a big pivot.


      ;The current question is how best to express abstractions like, for this many ticks, do this behavior.

      ;That runs into a problem because I'm having trouble reconciling the notion of "behaviour" with the notion of "component".

      ;What then is a "behaviour"
      ;  Right now, it's the update functions, which express changes to values on the entity, as well as express component additions and removals.  Though in theory they can do much more than this in a single operation because they can return entirely new entities, which may have many changes.  However, in practice, it may be safe to assume that components will be "well behaved", meaning that they only express their output as a series of crud functions on the input entity...

      ;The problem is that this is slow.  So we added mutability.  Crud functions can be mutable, meaning they transform their input.  This seemed to break some of the behavioural abstractions, which had been built upon the idea that a component could "behave like" another component by simulating it's effect on the input entity.  But now in the mutable case, those simulations are real.  So this breaks all of their logic.
      ;
      ;It raises the question of whether that logic needed to be their in the first place.  Maybe there are other perfectly good ways to do for-ticks, sequence, forever, etc...  
      ;Or mabye we need to pivot something about what a "handler" is.  As long as something like the syntax below works, that's fine.

      ;Maybe those don't need to happen at the component level.  
      ;
      ;If not, where?  What is a behaviour?  

      ;Spawn here is a component.
      ;It conveys the behaviour of, every tick spawn something.
      ;It's handler produces an entity with a (spawn ...) component attached.  In the mutable case, it actually attaches that spawn component.


      ;In v1: The natural abstraction we chose for for-ticks was (because it is taking a component as input) that it was going to add that component and then remove it after 200 ticks.

      ;In v2: I was trying to make for-ticks simulate that entity for some number of ticks.  I guess I thought it was going to be messy to add the component and then keep a reference to it so it could be yanked back. 

      ;Let's go back to v1, because I think it will be more resilient in the face of the mutability...

      (position 200 200
                #:update (update:position/x^ (curry + 1)))

        
      (sprite (h:circle 20 'solid 'red))


      (spawn-here (bullet 'green)
                  #:update (for-ticks 20))

      #;
      (for-ticks 200
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
                     (spawn-here (bullet 'blue)))))
      
      )

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









