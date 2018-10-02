#lang racket

(require "../game-entities.rkt")
(require "./animated-sprite.rkt")
(require "./detect-edge.rkt")
(require "./on-edge.rkt")
(require "./on-rule.rkt")
(require "../component-util.rkt")
(require "./counter.rkt")
(require "./spawn-once.rkt")
(require "../entity-helpers/sprite-util.rkt")
(require "../entity-helpers/movement-util.rkt")
(require "./backpack.rkt")
(require 2htdp/image)


(require posn
         threading)

(provide #;(struct-out backdrop)
         (rename-out [make-backdrop backdrop])
         backdrop-current-tile
         backdrop-tiles
         backdrop?
         
         bg->backdrop
         create-backdrop
         
         next-tile
         change-backdrop
         change-tile-to

         get-current-tile
         ;??
         get-total-tiles
         render-tile
         
         backdrop-eq?
         more-tiles?
         
         backdrop-edge-system
         player-edge-system
         spawn-all-from-backpack
         spawn-active-from-backpack
         tile-changed?
         not-tile-changed?
         first-tile?
         track-entities
         store-misplaced
         destroy-misplaced
         game->current-tile

         backdrop-width
         backdrop-height)

; ACTIVE ENTITIES

(define handler-function? (-> game? entity? entity?))

(struct backdrop (id tiles columns last-tile current-tile misplaced-entities))

(define (make-backdrop id tiles columns current-tile)
  (backdrop id tiles columns #f current-tile '()))

(define (add-backpack-if-no-backpack e)
  (if (not (get-component e backpack?))
      (add-component e (backpack))
      e))

(define (get-backdrop e)
  (get-component e backdrop?))

(define (get-trackable-entities g)
  (filter (has-component? active-on-bg?)
          (game-entities g)))

(define (track-entities g e)
  (set-backpack-entities
   e
   (get-trackable-entities g)))

#|
(detect-edge "player" 'left   (next-tile 'left))
(detect-edge "player" 'right  (next-tile 'right))
(detect-edge "player" 'top    (next-tile 'top))
(detect-edge "player" 'bottom (next-tile 'bottom))
|#

(define (spawn-all-from-backpack g e)
  (displayln "SPAWNING ALL FROM BACKPACK")
  (define backpack-entities (map item-entity (get-items e)))
  (displayln (~a "Backpack: " (map get-name backpack-entities)))
  (add-components e
                 (map (curry spawn-once #:relative #f) backpack-entities)))

(define (spawn-active-from-backpack g e)
  (displayln "SPAWNING ACTIVE ENTITIES FROM BACKPACK")
  (define current-tile-num (game->current-tile g))
  (define backpack-entities (filter (λ(ent)
                                      (can-be-on-tile? ent
                                                       current-tile-num))
                                    (map item-entity (get-items e))))
  (displayln (~a "ACTIVE: " (length backpack-entities)))
  (add-components e
                 (map (curry spawn-once #:relative #f) backpack-entities)))

(define (spawn-entities-on-tile-change g e)
  (define WIDTH (game-width g))
  (define HEIGHT (game-height g))
  (define p (get-component (get-entity "player" g) posn?)) ;need a better way to find the player without name
  (define pos-x (posn-x p))
  (define pos-y (posn-y p))
  (if (or (and (<= pos-x 0)      ((more-tiles? 'left) g e))
          (and (>= pos-x WIDTH)  ((more-tiles? 'right) g e))
          (and (<= pos-y 0)      ((more-tiles? 'top) g e))
          (and (>= pos-y HEIGHT) ((more-tiles? 'bottom) g e)))
      (spawn-active-from-backpack g e)
      e))


(define (first-tile? g e)
  (define bg-entity (get-backdrop-entity g))
  (define last-tile (backdrop-last-tile (get-component bg-entity backdrop?)))
  (eq? last-tile #f))

(define (tile-changed? g e)
  (define bg-entity (get-backdrop-entity g))
  (define last-tile (backdrop-last-tile (get-component bg-entity backdrop?)))
  (define current-tile (get-current-tile bg-entity))
  (not (eq? last-tile current-tile)))

(define (not-tile-changed? g e)
  (not (tile-changed? g e)))

(define (update-last-tile g e)
  (define last-tile (backdrop-last-tile (get-component e backdrop?)))
  (define current-tile (get-current-tile e))
  #;(and (not (eq? last-tile current-tile))
       (displayln (~a "Current Tile: " current-tile " | Last Tile: " last-tile)))
  (update-entity e backdrop? (struct-copy backdrop (get-component e backdrop?) [last-tile current-tile])))

(define (destroy-misplaced g e)
  (define misplaced-entities (game->misplaced-entities g))
  (and (not (empty? misplaced-entities))
            (displayln (~a "Misplaced: " (map get-name misplaced-entities))))
  (define bg-backdrop (get-component e backdrop?))
  (define new-backdrop (struct-copy backdrop bg-backdrop [misplaced-entities misplaced-entities]))
  (update-entity e backdrop? new-backdrop))
  
(define (update-backdrop g e c)
  (~> e
      ;(add-backpack-if-no-backpack _)
      ;(track-entities-if-not-tracking g _)
      ;(spawn-entities-on-tile-change g _)
      (update-last-tile g _)
      ))

(define (get-backdrop-entity g)
  (findf
    (has-component? backdrop?)
    (game-entities g)))

(new-component backdrop?
               update-backdrop)

(define (game->current-tile g)
  (get-current-tile
   (get-backdrop-entity g)))

(define (game->active-entities g)
  (define entities-to-track (get-trackable-entities g))

  (define current-tile-num (game->current-tile g))


  (define active-entities
    (filter (λ(e)
              (can-be-on-tile? e
                               current-tile-num))
            entities-to-track))

  active-entities)
  
(define (update-entities-list updated-entity current-entities)
  (define (f entity)
    (if (entity-eq? entity updated-entity)
        updated-entity
        entity))
  (map f current-entities))

(define (combine-trackable-entities a b)
  (define x (filter (λ (o) (not (member o b entity-eq?))) a))
  (define y (filter (λ (o) (member o  b entity-eq?)) a))
  (define z (filter (λ (o) (not (member o a entity-eq?))) b))
  ;(define (update-active-tile e)
  ;  (update-entity e active-on-bg? (make-active-on-bg tile)))
  (append x y z))

(define (game->misplaced-entities g)
  (define entities-to-track (get-trackable-entities g))

  (define current-tile-num (game->current-tile g))


  (define misplaced-entities
    (filter (λ(e)
              (not (can-be-on-tile? e
                                    current-tile-num)))
            entities-to-track))

  misplaced-entities)

(define (die-if-member-of doomed)
  (λ(e)
    (if (member e doomed entity-eq?)
        (add-component e (dead))
        e)))

(define (store-misplaced g e)
  (define backpack-entities (map item-entity (get-items e)))
  (define misplaced-entities (game->misplaced-entities g))
  (define new-backpack-entities (combine-trackable-entities misplaced-entities backpack-entities))
  (define updated-backpack (apply backpack new-backpack-entities))
  (update-entity e backpack? updated-backpack))

(define (clear-misplaced-entities e)
  (define updated-backdrop (struct-copy backdrop (get-backdrop e)
                                        [misplaced-entities '()]))
  (update-entity e backdrop? updated-backdrop))

(define (backdrop-end-of-frame g)
  (define WIDTH (game-width g))
  (define HEIGHT (game-height g))
  (define p (get-component (get-entity "player" g) posn?)) ;need a better way to find the player without name
  (define pos-x (posn-x p))
  (define pos-y (posn-y p))
  (define old-bg-entity (get-backdrop-entity g))
  
  (define bg-entity      
    (cond [(<= pos-x 0)      ((next-tile 'left)   g old-bg-entity)]
          [(>= pos-x WIDTH)  ((next-tile 'right)  g old-bg-entity)]
          [(<= pos-y 0)      ((next-tile 'top)    g old-bg-entity)]
          [(>= pos-y HEIGHT) ((next-tile 'bottom) g old-bg-entity)]
          [else old-bg-entity]))

  ;(define bg-entity-backpack (get-component bg-entity backpack?))

  (define entities-being-tracked
    (map item-entity (get-items bg-entity)))
  
  (define entities-to-stop-tracking
    (game-self-killed-entities g))

  (define new-entities-to-track
    (filter (λ(e)
              (not (member e entities-to-stop-tracking entity-eq?)))
            entities-being-tracked))

  (define misplaced-entities (backdrop-misplaced-entities (get-component bg-entity backdrop?)))
  
  (define new-bg-entity (update-entity
                         (clear-misplaced-entities bg-entity)
                         backpack?
                         (apply backpack new-entities-to-track)))
 
  

  (define new-entities (update-entities-list new-bg-entity (game-entities g)))
  
  (define new-entities-with-dead (map (die-if-member-of misplaced-entities) new-entities))
  
  (struct-copy game g
               [entities new-entities-with-dead]))

(new-game-function backdrop-end-of-frame)


; === POWERTOOLS ===
(define/contract (bg->backdrop bg #:rows rows #:columns columns #:start-tile [current 0])
  (-> image? #:rows integer? #:columns integer? #:start-tile integer? (listof any/c))
  (list
   (backdrop (random 1000000) (sheet->costume-list bg columns rows (* rows columns)) columns #f current '())
   (backpack)
   (on-rule first-tile? track-entities)
   (on-rule not-tile-changed? (do-many store-misplaced
                                       destroy-misplaced))

   (on-rule tile-changed? spawn-active-from-backpack)))

(define (backdrop-width b)
  (image-width  (first (backdrop-tiles (first b)))))

(define (backdrop-height b)
  (image-height  (first (backdrop-tiles (first b)))))

; === COMPONENTS ===
;separate create-backdrop component created to keep backdrop id field internal
(define/contract (create-backdrop tiles columns current-tile)
  (-> list? integer? integer? backdrop?)
  (backdrop (random 1000000) tiles columns current-tile))

; === HANDLER FUNCTIONS ===
(define/contract (next-tile direction)
  (-> symbol? handler-function?)
  (lambda (g e)
    (define backdrop         (get-component e backdrop?))
    (define total-tiles      (length (backdrop-tiles backdrop)))
    (define col              (backdrop-columns backdrop))
    (define current-bg-index (get-current-tile e))
    (define next-bg-index    (next-backdrop-index direction total-tiles col current-bg-index))
    (if  next-bg-index
        (update-entity ((set-current-tile next-bg-index) g e) ;(update-entity e counter? (counter next-bg-index))
                       animated-sprite? (new-sprite (pick-tile backdrop next-bg-index)))
        e)))

;Updates bg-backdrop component
(define/contract (change-backdrop backdrop)
  (-> backdrop? handler-function?)
  (lambda (g e)
    ((show-backdrop) g (update-entity e backdrop? backdrop))))

;Change backdrop tile 
(define/contract (change-tile-to num)
  (-> integer? handler-function?)
  (lambda (g e)
    (define bg-entity (get-entity "bg" g))
    (define backdrop (get-component bg-entity backdrop?))
    (update-entity ((set-current-tile num) g e) animated-sprite? (new-sprite (pick-tile backdrop num)))
    ))

;Renders start-tile from the bg-backdrop component
(define/contract (show-backdrop)
  (-> handler-function?)
  (lambda (g e)
    (define bg-component (get-component e backdrop?))
    ((change-sprite (new-sprite (render-tile bg-component))) g e)))

(define/contract (set-current-tile num)
  (-> integer? handler-function?)
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
    (if next-bg-index
        #t
        #f)))

;Compares id fields of backdrop components
(define (backdrop-eq? backdrop)
  (lambda (g e)
  (define bg-backdrop (get-component e backdrop?))
  (eq? (backdrop-id bg-backdrop) (backdrop-id backdrop))))

; === HELPER FUNCTIONS ===
(define/contract (get-current-tile e)
  (-> entity? integer?)
  (backdrop-current-tile (get-component e backdrop?)))

(define/contract (get-total-tiles e)
  (-> entity? integer?)
  (length (backdrop-tiles (get-component e backdrop?))))

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
                                    (+ current-backdrop-index col))]))

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



(struct active-on-bg (bg-list))

(provide (rename-out (make-active-on-bg active-on-bg))
         active-on-bg-bg-list
         active-on-random
         active-on-bg?
         set-active-on)

(define (make-active-on-bg . bg-list)
  (active-on-bg bg-list))
  
(define (can-be-on-tile? e n)
  (member n (active-on-bg-bg-list
             (get-component e active-on-bg?))))

#|
#;(define (update-active-on-bg g e c)
    (define num-or-list (active-on-bg-bg-list c))
    (define current-bg-index (if (get-component (get-entity "bg" g) backdrop?)
                                 (get-current-tile (get-entity "bg" g))
                                 (get-counter (get-entity "bg" g))))
    (define bg-list (if (list? num-or-list)
                        num-or-list
                        (list num-or-list)))
    (if (member current-bg-index bg-list)
        (remove-component e disabled?)
        (add-component (remove-component e disabled?)
                       (disabled))))

#;(new-component active-on-bg?
               update-active-on-bg)
|#

(define (active-on-random [min 0] [max #f])
  (lambda (g e)
    (define m (get-total-tiles (get-entity "bg" g)))
    (if (eq? max #f)
        (update-entity e active-on-bg? (make-active-on-bg (random min (add1 m))))
        (update-entity e active-on-bg? (make-active-on-bg (random min (add1 max)))))
    ))

(define (set-active-on . tiles)
  (lambda (g e)
    (update-entity e active-on-bg? (apply make-active-on-bg tiles))))
