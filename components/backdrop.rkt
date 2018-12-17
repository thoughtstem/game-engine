#lang racket

;Some examples...
(module+ test
  ;Trying to expose how backdrops get rendered and swap tiles...
  (require rackunit)
  (require "./on-start.rkt"
           "direction.rkt"
           "speed.rkt"
           "every-tick.rkt"
           "do-every.rkt"
           "key-movement.rkt"
           "../ai.rkt")
  

  (define red   (rectangle 10 10 'solid 'red))
  (define green (rectangle 10 10 'solid 'green))

  (define grid (above (beside red red   red)
                      (beside red green red)
                      (beside red red   red)))

  ;Old way
  (let ()

    (define b (bg->backdrop grid
                            #:rows       3
                            #:columns    3
                            #:start-tile 0))

    

    (define be (sprite->entity (render-tile b)  
                               #:name       "bg"
                               #:position   (posn 0 0)
                               #:components
                               (static)
                               b
                               (on-start (change-tile-to 4)) ;Swap to green tile on start...
                               ))

    (define g (initialize-game (list be)))


    (check-equal? red   (draw-entity be))
    (check-equal? green (draw-entity (tick-entity g be))))


  ;New way
  (let ()
    
    (define be (add-components
                (bg->backdrop-entity grid
                                     #:rows       3
                                     #:columns    3
                                     #:start-tile 0
                                     #:scale 2)
                (on-start (change-tile-to 4))))

    (define g (initialize-game (list be)))

    (check-equal?  (efficient-backdrop? be) #t)
    (check-equal? (scale 2 red)   (draw-entity be))
    (check-equal? (scale 2 green) (draw-entity (tick-entity g be)))

    ;(start-game be)
    )

  (let ()

    (define be (bg->backdrop-entity grid
                                    #:rows       3
                                    #:columns    3
                                    #:start-tile 0))


    (define player (sprite->entity (square 1 'solid 'black)
                                   #:name "player"
                                   #:position (posn 5 5)
                                   #:components
                                   (speed 1)
                                   (direction 0)
                                   (every-tick (move))
                                   (player-edge-system)))

    (define g (initialize-game (list player be)))

    (define ticked-g      (tick g #:ticks 4))
    (define ticked-player (get-entity "player" ticked-g))

    (check-equal? (round (x ticked-player)) 9)
    (check-equal? (game->current-tile ticked-g) 0) ;Not crossed threshold yet

    (define ticked-g2      (tick ticked-g #:ticks 2))
    (define ticked-player2 (get-entity "player" ticked-g2))   

    (check-equal? (round (x ticked-player2)) 2)     ;Wraparound
    (check-equal? (game->current-tile ticked-g2) 1) ;Crossed threshold yet


    )
  

  (let ()

    

    ;Player is on tile 2
    ; Spawn something with    (active-on-bg 0)
    ; Want: That thing to get (active-on-bg 2)

    
    ;Currently: Ends up on tile 0
    
    (define be (bg->backdrop-entity grid
                                    #:rows       3
                                    #:columns    3
                                    #:start-tile 2 ;Start on 2
                                    ))

    
    (define to-spawn (sprite->entity (square 1 'solid 'yellow)
                                     #:name "spawnee"
                                     #:position (posn 0 0)
                                     #:components
                                     (make-active-on-bg 0) ;Incorrectly specify active tile
                                     ))


    (define player (sprite->entity (square 1 'solid 'black)
                                   #:name "player"
                                   #:position (posn 5 5)
                                   #:components
                                   (on-start (spawn to-spawn))))

    (define g (initialize-game (list player be)))
    (define ticked-g      (tick g #:ticks 4))

    (define spawned (get-entity "spawnee" ticked-g))

    (check-equal? (length (game-entities ticked-g))
                  3)

    (check-equal? (first (active-on-bg-bg-list (get-component spawned active-on-bg?)))
                  2)))

(require "../game-entities.rkt")
(require "./animated-sprite.rkt")
(require "./detect-edge.rkt")
(require "./on-edge.rkt")
(require "./on-rule.rkt")
(require "../component-util.rkt")
(require "./spawn-once.rkt")
(require "../entity-helpers/sprite-util.rkt")
(require "../entity-helpers/movement-util.rkt")
(require "./backpack.rkt")
(require 2htdp/image)

(require posn
         threading)

(provide (rename-out [make-backdrop backdrop])
         backdrop?
         backdrop-current-tile
         backdrop-tiles
         backdrop-columns

         game->tracking-entity

         backdrop-id
         
         next-tile
         change-backdrop
         change-tile-to
         
         backdrop-eq?
         more-tiles?
         tile-changed?

         render-tile
         backdrop-width
         backdrop-height
         backdrop-length
         bg->backdrop
         bg->backdrop-entity
         game->current-tile
         player-edge-system)

;====== BACKDROP =====
;  Keeps track of the various tiles, also manages which entities should be active on which tiles
;  We call those entities "trackable entities".  More specifically, they have active-on-bg components attached.

(struct backdrop (id tiles columns last-tile current-tile misplaced-entities))
(struct active-on-bg (bg-list) #:transparent)

; === POWERTOOLS ===
(define/contract (bg->backdrop bg #:rows       rows
                                  #:columns    columns
                                  #:start-tile [current 0]
                                  #:scaler     (scaler 1))
  ; #:mini-map (yes/no) #:key
  (-> image? #:rows integer? #:columns integer? #:start-tile integer? (listof any/c))
  (define backdrop-component
    (backdrop (random 1000000)
              (sheet->costume-list bg columns rows (* rows columns))
              columns #f current '()))
  (list
   backdrop-component
   (precompiler
    (backdrop-tiles backdrop-component))
   (backpack)))



(define (bg->backdrop-entity i
                             #:rows       (rows 3)
                             #:columns    (cols 3)
                             #:start-tile (start-tile 0)
                             #:scale      (scale 1))
  
  (define b (bg->backdrop i
                          #:rows       rows
                          #:columns    cols
                          #:start-tile start-tile))

  (define backdrop-animated-sprite
    (set-scale-xy scale
                  (sheet->sprite (apply beside (backdrop-tiles (first b)))
                                 #:rows 1
                                 #:columns    (* rows cols)
                                 #:row-number 1
                                 #:animate?    #f)))
  
  (define be (sprite->entity backdrop-animated-sprite
                             #:name       "bg"
                             #:position   (posn 0 0)
                             #:components
                             (static)
                             b))
  
  be)







;The backdrop structs have the following associated entity predicates
(define trackable-entity? (has-component? active-on-bg?))
(define tracking-entity?  (and/c (has-component? backdrop?)
                                 (has-component? backpack?)))


;Getters
(define (backdrop-width b)
  (image-width  (first (backdrop-tiles (first b)))))

(define (backdrop-height b)
  (image-height  (first (backdrop-tiles (first b)))))

(define (backdrop-length b)
  (length (backdrop-tiles (first b))))

;Constructor for backdrop
(define (make-backdrop id tiles columns current-tile)
  (backdrop id tiles columns #f current-tile '()))

;Convenience getter for tracking entity with a backdrop 
(define (get-backdrop e)
  (get-component e backdrop?))

;Way of collecting all trackable entities in a game
(define (game->trackable-entities g)
  (-> game? (listof trackable-entity?))
  (filter trackable-entity?
          (game-entities g)))

;Usually, there is only one backdrop in a game, so it should be
; easy to find it. 
(define (game->tracking-entity g)
  (-> game? tracking-entity?)
  
  (findf tracking-entity?
         (game-entities g)))

;It can be convenient to work with tracking entities, not backdrop structs.
;These functions help:

;Given some tracking entity, we can extract the current tile
(define/contract (tracking-entity->current-tile e)
  (-> tracking-entity? integer?)
  (backdrop-current-tile (get-backdrop e)))

;Or the last tile
(define/contract (tracking-entity->last-tile e)
  (-> tracking-entity? (or/c integer? boolean?))
  (backdrop-last-tile (get-backdrop e)))

;Or the total tiles
(define/contract (tracking-entity->total-tiles e)
  (-> tracking-entity? integer?)
  (length (backdrop-tiles (get-component e backdrop?))))

;Or if it's a trackable entity, the tiles it should be shown on.
(define/contract (trackable-entity->active-tiles e)
  (-> trackable-entity? (listof integer?))
  (active-on-bg-bg-list (get-component e active-on-bg?)))

;======TRACKED ENTITIES======
;Whether something is "tracking" or "trackable" is simply a matter of what
;  components the entity has.

;But there is another state: "tracked", which happens at runtime.
;  It means there exists a tracking entity actually knows about the trackable entity.
;  Then it's "tracked".

;Tracked entities are stored in a backpack on a tracking entity
(define (entity->tracked-entities e)
  (-> tracking-entity? (listof trackable-entity?))
  (get-backpack-entities e))




;=======CURRENT TILES AND WHEN TO SHOW THINGS====

;It can be convenient to work at the level of the game,
;  without having the tracking entity in hand.
;(Works because it's assumed the tracking entity is a singleton.)

(define (game->current-tile g)
  (tracking-entity->current-tile
   (game->tracking-entity g)))

(define (game->last-tile g)
  (tracking-entity->last-tile
   (game->tracking-entity g)))

;We should show a tracked entity when its active tile(s) matches the
;  game's current tile
(define (should-be-shown? g e)
  (-> game? trackable-entity? boolean?)
  (member (game->current-tile g)
          (trackable-entity->active-tiles e)))


(define (game->entities-to-show g)
  (-> game? trackable-entity?)

  (filter (curry should-be-shown? g)
          (entity->tracked-entities
           (game->tracking-entity g))
          ))

;=====????====

;It's the first tile we've been to if there's no previous tile
(define (first-tile? g e)
  (define last-tile (game->last-tile g))
  (eq? last-tile #f))

(define (tile-changed? g e)
  (not (eq? (game->last-tile g)
            (game->current-tile g))))


(define (current-player-boundary g)
  (define WIDTH  (game-width g))
  (define HEIGHT (game-height g))
  
  (define p (get-component (get-entity "player" g) posn?)) ;need a better way to find the player without name

  (define pos-x (posn-x p))
  (define pos-y (posn-y p))
  
  (cond [(<= pos-x 0)      'left]
        [(>= pos-x WIDTH)  'right]
        [(<= pos-y 0)      'top]
        [(>= pos-y HEIGHT) 'bottom]
        [else #f]))


(define (tile-will-change? g e)
  (-> game? entity? (or/c boolean? symbol?))
  (and (get-entity "player" g)
       (current-player-boundary g)))


;For some tracking entity, sometimes we need to update its backdrop's
;  last tile (i.e. when the player moves to a new tile)
(define (update-last-tile g e)
  (define current-tile (game->current-tile g))
  
  (define new-backdrop
    (struct-copy backdrop
                 (get-component e backdrop?)
                 [last-tile current-tile]))
  
  (update-entity e backdrop? new-backdrop))

;Currently, updating the last tile is the only thing the
;  backdrop component needs to ensure that it does.
(define (update-backdrop g e c)
  (update-last-tile g e))

;Here, we register the backdop component
(new-component backdrop?  update-backdrop)


;======SPAWNING TRACKED ENTITIES======


(define (game->misplaced-entities g)
  (filter (and/c
           trackable-entity?
           (not/c (curry should-be-shown? g)))
          (game-entities g)))





(define (backdrop-end-of-frame g)
  (if (or (not (game->tracking-entity g))
          (not (get-entity "player" g)))
      g
      (backdrop-end-of-frame-for-real g)))


(define (end-of-frame-tile-changed g old-tracking-entity dir)

  (define tracking-entity
    ((next-tile dir) g old-tracking-entity))
  
  (define new-entities
    (replace-entity-in-list tracking-entity (game-entities g)))
  
  (struct-copy game g
               [entities new-entities]))



(define (track-new-entities-if-any g old-tracking-entity)
  ;Some of these trackable entities might be new.  Start tracking, if so.
  (define all-trackable     (game->trackable-entities g))
  (define currently-tracked (entity->tracked-entities old-tracking-entity))

  ;If they are new, override their active-on-bg to current tile.
  (define (override-active-on-bg e)
    (if (member e currently-tracked entity-eq?)
        e
        (update-entity e active-on-bg?
                       (make-active-on-bg (game->current-tile g)))))

  (set-backpack-entities old-tracking-entity
                         (union-entities (map override-active-on-bg all-trackable)
                                         currently-tracked))



  #;(set-backpack-entities old-tracking-entity
                           (union-entities just-started-tracking
                                           currently-tracked)))


(define (stop-tracking-dead-entities g old-tracking-entity)
  (define entities-to-stop-tracking
    (game-self-killed-entities g))

  (define new-entities-to-track
    (filter (curryr entity-in-list? entities-to-stop-tracking)
            (entity->tracked-entities old-tracking-entity)))
  
  (set-backpack-entities old-tracking-entity
                         new-entities-to-track))

(define  (kill-if-living-and-on-wrong-tile g tracking-entity es)
  (define misplaced-entities
    (filter (not/c (curry should-be-shown? g))
            (entity->tracked-entities tracking-entity)))
 
  (define es-with-dead
    (map (curryr die-if-member-of misplaced-entities) es))
  
  es-with-dead)

(define  (spawn-if-dead-but-on-right-tile g tracking-entity es)
  (define to-spawn
    (filter (and/c (curry should-be-shown? g)
                   (not/c (curryr member es)))
            (entity->tracked-entities tracking-entity)))

  (define new-tracking-entity
    (spawn-many-from #:relative #f
                     tracking-entity to-spawn))
  

  
  
  (replace-entity-in-list new-tracking-entity es))


(define (stop-tracking-no-longer-trackables g tracking-entity)
  
  ;Can we make these four operations clearer and in an obvious sequence??
  ;  Also, need to handle case where we find a thing without an active-on-bg, but which used to have one (we are currently tracking...)
  
  (define all-non-trackable-es
    (filter (not/c trackable-entity?)
            (game-entities g)))

  (define tracking
    (entity->tracked-entities tracking-entity))

  (define stop-tracking
    (filter (curryr member all-non-trackable-es entity-eq?)
            tracking))

  (define new-tracking
    (filter (not/c (curryr member stop-tracking entity-eq?))
                   tracking))
  

  (set-backpack-entities tracking-entity new-tracking))

(define (end-of-frame-no-tile-changed g old-tracking-entity)

  (define new-tracking-entity
    (~> old-tracking-entity
        (stop-tracking-no-longer-trackables g _)
        (track-new-entities-if-any          g _)
        (stop-tracking-dead-entities        g _)
        ))
 
  (define entities-with-new-tracker
    (replace-entity-in-list new-tracking-entity
                            (game-entities g)))

  (define new-entities
    (~> entities-with-new-tracker
        (kill-if-living-and-on-wrong-tile g new-tracking-entity _)
        (spawn-if-dead-but-on-right-tile  g new-tracking-entity _)))
  
  (struct-copy game g
               [entities new-entities]))



(define (backdrop-end-of-frame-for-real g)

  ;Current tracking entity
  (define old-tracking-entity (game->tracking-entity g))

  ;Direction of tile change (will be #f if not moving tiles this frame)
  (define dir (tile-will-change? g old-tracking-entity))


  
  (if dir
      (end-of-frame-tile-changed    g old-tracking-entity dir)
      (end-of-frame-no-tile-changed g old-tracking-entity)))


(new-game-function backdrop-end-of-frame)




; === HANDLER FUNCTIONS ===
(define/contract (next-tile direction)
  (-> symbol? (-> game? entity? entity?))
  (lambda (g e)
    (define backdrop         (get-component e backdrop?))
    (define total-tiles      (length (backdrop-tiles backdrop)))
    (define col              (backdrop-columns backdrop))
    (define current-bg-index (tracking-entity->current-tile e))
    (define next-bg-index    (next-backdrop-index direction total-tiles col current-bg-index))
    (if  next-bg-index
         ((change-tile-to next-bg-index) g e)
        #;(update-entity ((set-current-tile next-bg-index) g e) ;(update-entity e counter? (counter next-bg-index))
                       animated-sprite? (new-sprite (pick-tile backdrop next-bg-index)))
        e)))

;Updates bg-backdrop component. Inputs new background img. Num col and rows will stay the same for a new backdrop!
; Function only replaces list of tiles stored in the struct.
(define (change-backdrop new-bg)
  ;(-> image? handler-function?)
  (lambda (g e)
    (define bg-entity   (get-entity "bg" g))
    (define bg-backdrop (get-component bg-entity backdrop?))
    (define columns     (backdrop-columns bg-backdrop))
    (define rows        (/ (length (backdrop-tiles bg-backdrop)) columns))
    (define new-tiles   (sheet->costume-list new-bg columns rows (* rows columns)))
    (define new-bg-backdrop   (struct-copy backdrop bg-backdrop [id (random 1000000)] [tiles new-tiles]))
    ((show-backdrop) g (update-entity e backdrop? new-bg-backdrop))))

;Renders start-tile from the bg-backdrop component
(define (show-backdrop)
  (lambda (g e)
    (define bg-component (get-component e backdrop?))
    ((change-sprite (new-sprite (render-tile bg-component))) g e)))

;Change backdrop tile 
(define/contract (change-tile-to num)
  (-> integer? (-> game? entity? entity?))
  (lambda (g e)
    (define bg-entity (get-entity "bg" g))
    (define backdrop  (get-component bg-entity backdrop?))


    (define new-e ((set-current-tile num) g e))

    (define current-sprite (get-component e animated-sprite?))

    (define the-new-sprite
      (if (efficient-backdrop? e)                  ;Check if it's our new way of managing backdrops
          (set-frame current-sprite num)           ;Update animation frame...
          (new-sprite (pick-tile backdrop num))))  ;If not, do it the old (SLOOOW) way

    (update-entity new-e animated-sprite? the-new-sprite)))


(define (efficient-backdrop? e)
  (< 1 (vector-length (animated-sprite-frames (get-component e animated-sprite?)))))


(define/contract (set-current-tile num)
  (-> integer? (-> game? entity? entity?))
  (lambda (g e)
    (define bg-entity (get-entity "bg" g))
    (define bg-backdrop (get-component bg-entity backdrop?))
    (update-entity bg-entity backdrop? (struct-copy backdrop bg-backdrop
                                           [current-tile num]))))

; === RULES ===
; Assumes background is named "bg" and has a backdrop component
(define (more-tiles? direction)
  (lambda (g e)
    (define backdrop         (get-component (get-entity "bg" g) backdrop?))
    (define total-tiles      (length (backdrop-tiles backdrop)))
    (define col              (backdrop-columns backdrop))
    (define current-bg-index (backdrop-current-tile backdrop))
    (define next-bg-index    (next-backdrop-index direction total-tiles col current-bg-index))
    (if next-bg-index  #t  #f)))

;Compares id fields of backdrop components
(define (backdrop-eq? backdrop)
  (lambda (g e)
  (define bg-backdrop (get-component e backdrop?))
  (eq? (backdrop-id bg-backdrop) (backdrop-id backdrop))))

; === HELPER FUNCTIONS ===

(define/contract (render-tile backdrop)
  (-> (or/c backdrop? list?) image?)
  (if (list? backdrop)
      (render-tile (first backdrop))
      (pick-tile backdrop (backdrop-current-tile backdrop))))

(define/contract (pick-tile backdrop i)
  (-> backdrop? integer? image?)
  (list-ref (backdrop-tiles backdrop) i))

(define/contract (next-backdrop-index direction total-tiles col current-backdrop-index)
  (-> symbol? integer? integer? integer? (or/c integer? boolean?))  
  (define left-edge-list   (range 0 total-tiles col))
  (define right-edge-list  (range (sub1 col) total-tiles col))
  (define top-edge-list    (range 0  col))
  (define bottom-edge-list (range (- total-tiles col) total-tiles))
  (cond [(eq? direction 'left)  (if (member current-backdrop-index left-edge-list)
                                    #f
                                    (sub1 current-backdrop-index))]
        [(eq? direction 'right) (if (member current-backdrop-index right-edge-list)
                                    #f
                                    (add1 current-backdrop-index))]
        [(eq? direction 'top)    (if (member current-backdrop-index top-edge-list)
                                    #f
                                    (- current-backdrop-index col))]
        [(eq? direction 'bottom)    (if (member current-backdrop-index bottom-edge-list)
                                    #f
                                    (+ current-backdrop-index col))]
        [else (error (~a "What direction is " direction))]))

;=== SYSTEMS ===
;These are collections of components to help with flip-screen navigation

; Backdrop Edge System
; Requires a player to be named "player"
; Should be added to a background entity with a backdrop component
(define (backdrop-edge-system)
  (list (detect-edge "player" 'left   (next-tile 'left))
        (detect-edge "player" 'right  (next-tile 'right))
        (detect-edge "player" 'top    (next-tile 'top))
        (detect-edge "player" 'bottom (next-tile 'bottom))))

; Player Edge System
; Assumes there is background entity named "bg" with a backdrop component
; Should be added to the player entity
(define (player-edge-system)
  (list (on-edge 'left   #:rule tile-changed?  (go-to-pos-inside 'right))
        (on-edge 'right  #:rule tile-changed?  (go-to-pos-inside 'left))
        (on-edge 'top    #:rule tile-changed?  (go-to-pos-inside 'bottom))
        (on-edge 'bottom #:rule tile-changed?  (go-to-pos-inside 'top))))


(provide (rename-out (make-active-on-bg active-on-bg))
         active-on-bg-bg-list
         active-on-random
         active-on-bg?
         set-active-on)

(define/contract (make-active-on-bg . bg-list)
  (->* () () #:rest (listof number?) active-on-bg?)
  (active-on-bg bg-list))

(define (active-on-random [min 0] [max #f])
  (lambda (g e)
    (define m (tracking-entity->total-tiles (game->tracking-entity g)))
    (if (eq? max #f)
        (update-entity e active-on-bg? (make-active-on-bg (random min (add1 m))))
        (update-entity e active-on-bg? (make-active-on-bg (random min (add1 max)))))))

(define (set-active-on . tiles)
  (lambda (g e)
    (update-entity e active-on-bg? (apply make-active-on-bg tiles))))
