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

(pretty-print-game to-play)
(play to-play) 




;TODO: Conway is fast now.  The game is still slow.  
;   I've verified that it's all the entities that are being updated -- because I took them out, and the FPS was back at 50+

;  Look for ways to optimize.  Mutability during entity/component ticks??
;  Toggle off the game's struct copy optionally?  Abandon that except for during tests? 


; The weird thing is that i've made entity and component updates mutable, and there's been NO changed in the framerate.  The fact that there's none AT ALL is suprising to me.  Where is the slowdown then?  And are those functions not getting exercised at all?


;TODO: Refactor the game loop.
;Game loop
;  Not copying the game struct gave no improvement.  (Perhaps copying is not as expensive as I thought?)
;  Commenting out the op application got only 3 FPS improvement
;  Commenting out the op generation got us to 20+ FPS.  Surprised that it gave us any.  But then, when I saw that it did, I was surprised it didn't get us back to 60. 
;
;  The culprit in the for loops must be fetching the (component-update c).  
      ;Specifically, it seems to be the contract!
  ;(-> component? (or/c #f handler?))

;  Empty for loops get us to 40+
     ;Is it even doing anything???

;  Taking out contracts gets us to 48-51

;  An empty mode-lambda tick (not even looping over the entities to find sprites) gets us to 60, which apears to be the max.



;Things to optimize:

;  Mode-lambda: looking at sprites

;  Render tick:
;     Generating the handler
;     Applying the handler
;     CRUD ops and mutability (but maybe revisit mutability after figuring out why some of the reading is slow -- that won't get faster by making things mutable, so we should see what we're doing inefficiently there.
;     Contracts 

;Maybe retain these comments and rollback to last commit, verify above and procede to optmize the most important things first







;Confirmations:
; 3 FPS base
;
; No nested-for loops: 30-40?
; No entity->sprite: 3 FPS

; No nested-for loops or entity->sprite: 60+


; Working on entity->sprite first.  Seems like 
;   it would be the fastest if it didn't have to re-collect the sprites every tick.  If it just had a list of sprites that was getting mutably updated...
;    But maybe it's all the ml:sprite constructions, actually...
;      If we don't construct so many, it is faster.  But could that be because of the (x ...) and (y ...) getters, or the real->double-flonum, or the construction itself?

;  Maybe losing about 10 FPS on getting entity x y.  Not clear to me how to optimize this or anything in the rendering.  Maybe there's a way to not ahve to loop over all entities and construct all new sprites.  Maybe we can somehow know which ones have changed and reuse last tick's sprites for the ones that haven't.  BUt for now let's focus on the more major causes of slowdown -- in the runtime tick



;Somehow filter out components that have no update functions.  Just wasted noops.

;Looking into apply-op now.  The major source of slowdown -- updating entities in the game (update-entity g ...).  Expensive...

;   Ummm.  Noo.... making apply-op into a noop only gives another 2 FPS...
;     Is it just getting the op at all that's slow??



;Yes.  Getting an op means fetching a handler and passing in the g e c, and getting back an e

;  If all 1000 entities have a #f handler, we get 30-40 FPS
;  If they are all noops, it's 20~ -- which is annoying.  Just function call overhead?
;     Might be contracts on define-component functions...
;  How to make these less expensive in general?



;Current plan.  Try to tackle the efficiency of queries -- get-component and get-entity.  Benchmark the slowdown and fix if it's worth it.  If not, where is the extra overhead coming from now that updates are fast.


; Memoize gave a couple more FPS, but it's also broke things.  Why?
;  equal? vs eq?
;  actually that slows it down, so does that mean the query caching strategy is doomed to failure?  Or can we do it better than memoize?


; Consider looking at renderer tick again and look for shared memory opportunities
;   First try indexing in to find the sprite instead of querying


;Suuuuper close to 30 FPS
;   Can we get renderer to be faster?  i.e. by not always reconstructing a new sprite.  If we had a "dirty bit" in e, then we could return the last.

;   Ooooor is it not worth it?
;      Maybe we've optimized enough.




;Okay, its at 30 now (on 500 entities -- it was never 1000)
;  Now can we take stock of all optimizations, make them optional and make sure the tests 
;  run correctly both when there are optimizations and not.


;make a mutable-state toggle parameter.  Update crud accordingly.

;  entity components and game entities should be vectors, not lists?  (Experiment: Would make updates faster, but adds and removals slower, maybe...)


