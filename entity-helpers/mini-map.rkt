#lang racket

(provide open-mini-map
         mini-map)

(require "../game-entities.rkt"
         "../component-util.rkt"
         "../components/animated-sprite.rkt"
         ;"../components/detect-edge.rkt"
         ;"../components/on-edge.rkt"
         "../components/on-rule.rkt"
         "../components/backdrop.rkt"
         "../components/on-start.rkt"
         "../components/spawn-once.rkt"
         "../components/on-key.rkt"
         ;"../components/after-time.rkt"
         "../components/counter.rkt"
         "../components/observe-change.rkt"
         "../entity-helpers/sprite-util.rkt"
         "../entity-helpers/movement-util.rkt"
         2htdp/image
         posn
         memoize
         threading)

(define handler-function? (-> game? entity? entity?))
(define rule?             (-> game? entity? boolean?))

; You can specify mini-map scale and frame width here
(define mini-map-scale 0.075)
(define frame-width 6)


; ========== MINI-MAP REFACTOR =============

(define/contract (draw-mini-map backdrop x-scale y-scale)
  (-> backdrop? number? number? image?)
  (define tiles      (backdrop-tiles backdrop))
  (define columns    (backdrop-columns backdrop))
  (define rows       (/ (length tiles) columns))

  (define tile-width  (image-width  (first tiles)))
  (define tile-height (image-height (first tiles)))
  
  (define mini-map (scale/xy x-scale y-scale
                             (mini-map-base tiles
                                            columns rows
                                            tile-width tile-height
                                            (length tiles))))
  
  (frame-mini-map (scale mini-map-scale mini-map)))

(define (map-frame-x tile backdrop)
  (define tiles      (backdrop-tiles backdrop))
  (define columns    (backdrop-columns backdrop))
  (- (modulo   tile columns) (/ (sub1 columns) 2)))

(define (map-frame-y tile backdrop)
  (define tiles      (backdrop-tiles backdrop))
  (define columns    (backdrop-columns backdrop))
  (define rows       (/ (length tiles) columns))
  (- (quotient tile columns) (/ (sub1 rows) 2)))

(define/contract (mini-map-frame-sprite backdrop tile-index map-base)
  (-> backdrop? integer? image?  sprite?)

  (define map-width (- (image-width map-base) frame-width))
  (define map-height (- (image-height map-base) frame-width))
  
  (define tiles      (backdrop-tiles backdrop))
  (define columns    (backdrop-columns backdrop))
  (define rows       (/ (length tiles) columns))

  (define tile-width  (/ map-width columns))
  (define tile-height (/ map-height rows))

  (define frame-x (map-frame-x tile-index backdrop))
  (define frame-y (map-frame-y tile-index backdrop))
  
  (define frame (overlay (rectangle tile-width tile-height 'outline 'red)
                         (rectangle (+ 2 tile-width) (+ 2 tile-height) 'solid 'transparent)))
  (new-sprite  frame
               #:x-offset (* frame-x tile-width)
               #:y-offset (* frame-y tile-height)))

(define (mini-map bg-ent #:close-key close-key)
  (define bg-as (get-component bg-ent animated-sprite?))
  (define backdrop   (get-component bg-ent backdrop?))
  (define bg-x-scale (get-x-scale bg-as))
  (define bg-y-scale (get-y-scale bg-as))
  (define tile-index (backdrop-current-tile backdrop))

  (define map-base-image (draw-mini-map backdrop bg-x-scale bg-y-scale))
  (define map-base-sprite (new-sprite map-base-image))
  (define map-frame-sprite (mini-map-frame-sprite backdrop tile-index map-base-image))

  (define id         (backdrop-id backdrop))

  (define mini-map-offset-x (* -0.05 (width bg-ent)))
  (define mini-map-offset-y (* -0.05 (height bg-ent)))
      
  (define (update-mini-map-frame g e)
    (define as (get-component e (curry component-eq? map-frame-sprite)))
    (define backdrop   (get-component (game->tracking-entity g) backdrop?))
    (define tile (backdrop-current-tile backdrop))
    (define frame-image (render as))
    (define tile-width (- (image-width frame-image) 2))
    (define tile-height (- (image-height frame-image) 2))
      
    (define new-x-offset (* tile-width (map-frame-x tile backdrop)))
    (define new-y-offset (* tile-height (map-frame-y tile backdrop)))
    (define new-as (~> as
                       (set-x-offset new-x-offset _)
                       (set-y-offset new-y-offset _)))
    (update-entity e (curry component-eq? as) new-as))

  (define (update-mini-map g e)
    (define old-map-as (get-component e (curry component-eq? map-base-sprite)))
    
    (define new-backdrop (get-component (game->tracking-entity g) backdrop?))
    (define new-bg-as (get-component bg-ent animated-sprite?))
    (define new-bg-x-scale (get-x-scale bg-as))
    (define new-bg-y-scale (get-y-scale bg-as))
    (define new-map-base (draw-mini-map new-backdrop bg-x-scale bg-y-scale))
    (define new-frames (vector (fast-image new-map-base)))
    (define new-map-as (struct-copy animated-sprite old-map-as
                                    [frames new-frames]
                                    [o-frames new-frames]))
    (update-entity e (curry component-eq? old-map-as) new-map-as))
    
  (sprite->entity (list map-frame-sprite
                        map-base-sprite)
                  #:name       "mini-map"
                  #:position   (posn 0 0)
                  #:components (hidden)
                  (static)
                  (layer "ui")
                  (counter id)
                  (on-start (do-many (go-to-pos-inside 'bottom-right
                                                       #:posn-offset (posn mini-map-offset-x
                                                                           mini-map-offset-y))
                                     show))
                  (on-key close-key die)
                  (on-rule tile-changed? update-mini-map-frame)
                  (observe-change backdrop-changed? (if/r backdrop-changed?
                                                          (do-many (λ (g e)(displayln "==== BACKDROP CHANGED ====") e)
                                                                   update-mini-map)
                                                          ))
                  ))

; allows to add mini-map entity to the game.
; Requires game to have an entity with a backdrop component.
; Can be called from any entity: (on-key "m" (open-mini-map #:close-key 'o))
(define (open-mini-map #:close-key close-key)
  (-> #:close-key (or/c symbol? string?) handler-function?)
  (lambda (g e)
    (define bg-ent (game->tracking-entity g))
    (define mini-map-entity (mini-map bg-ent #:close-key close-key))
    (if (get-entity "mini-map" g)
        e
        (add-components e (spawn-once mini-map-entity #:relative? #f)))))

(define (backdrop-changed? g e)
  (backdrop-id (get-component (game->tracking-entity g) backdrop?)))

; ========== END OF MINI-MAP REFACTOR ===========

#|      
; allows to add mini-map entity to the game.
; Requires game to have an entity with a backdrop component.
; Can be called from any entity: (on-key "m" (open-mini-map #:close-key 'o))
(define/contract (open-mini-map #:close-key close-key)
  (-> #:close-key (or/c symbol? string?) handler-function?)
  (lambda (g e)
    (define backdrop   (get-component (game->tracking-entity g) backdrop?))
    (define tile-index (backdrop-current-tile backdrop))
    
    (define mini-map-l (mini-map-layout backdrop tile-index))
    (define mini-map-f (mini-map-frame backdrop tile-index mini-map-l))
    
    (define id         (backdrop-id backdrop))

    (define mini-map-offset-x (* -0.05 (game-width g)))
    (define mini-map-offset-y (* -0.05 (game-height g)))
    
    (define mini-map-layout-entity
      (sprite->entity mini-map-l
                      #:name       "mini-map-layout"
                      #:position   (posn 0 0)
                      #:components (hidden)
                                   (static)
                                   (counter id)
                                   (on-start (do-many (go-to-pos-inside 'bottom-right)
                                                      (change-x-by mini-map-offset-x)
                                                      (change-y-by mini-map-offset-y)
                                                      show))
                                   (on-key close-key die)
                                   (observe-change backdrop-r? (λ(g e1 e2)
                                                                 ;(get-component (game->tracking-entity g) backdrop?)
                                                                 ;(define tile-index (backdrop-current-tile backdrop))
                                                                (define backdrop     (get-component (game->tracking-entity g) backdrop?))
                                                                (define current-tile (backdrop-current-tile backdrop))
                                                                (if (backdrop-r? g e2)
                                                                    (update-entity e2 animated-sprite? (new-sprite (mini-map-layout backdrop current-tile)))
                                                                    (if (void? e1)
                                                                        e2
                                                                        (update-entity e2 animated-sprite? (new-sprite (mini-map-layout backdrop current-tile)))))))))
    (define mini-map-frame-entity
      (sprite->entity mini-map-f
                      #:name       "mini-map-frame"
                      #:position   (posn 0 0)
                      #:components (hidden)
                                   (static)
                                   (on-start (do-many (go-to-pos-inside 'bottom-right)
                                                      (change-x-by (+ (/ frame-width -2) mini-map-offset-x))
                                                      (change-y-by (+ (/ frame-width -2) mini-map-offset-y))
                                                      show))
                                   (on-key close-key die)
                                   (on-rule tile-changed? (update-mini-map-frame mini-map-l))
                                   ))

    (if (get-entity "mini-map-layout" g)
        e
        (add-components e (spawn-once mini-map-layout-entity #:relative? #f)
                          (spawn-once mini-map-frame-entity #:relative? #f)))))

; create an image for a mini-map entity animated-sprite
(define/contract (mini-map-layout backdrop tile-index)
  (-> backdrop? integer? image?)
  (define tiles      (backdrop-tiles backdrop))
  (define columns    (backdrop-columns backdrop))
  (define rows       (/ (length tiles) columns))

  (define tile-width  (image-width  (first tiles)))
  (define tile-height (image-height (first tiles)))

  (define frame-x (modulo   tile-index columns))
  (define frame-y (quotient tile-index columns))
  
  (define frame (rectangle tile-width tile-height "outline" "red")) 
  (define mini-map (mini-map-base tiles
                                  columns rows
                                  tile-width tile-height
                                  (length tiles)))
  
  (frame-mini-map (scale mini-map-scale mini-map)))

(define/contract (mini-map-frame backdrop tile-index layout)
  (-> backdrop? integer? image? image?)
  (define empty-image (rectangle (+ (- frame-width) (image-width layout)) (+ (- frame-width) (image-height layout)) "solid" (make-color 0 0 0 0)))
  
  (define tiles      (backdrop-tiles backdrop))
  (define columns    (backdrop-columns backdrop))
  (define rows       (/ (length tiles) columns))

  (define tile-width  (/ (image-width  empty-image) columns))
  (define tile-height (/ (image-height empty-image) rows))

  (define frame-x (modulo   tile-index columns))
  (define frame-y (quotient tile-index columns))
  
  (define frame (rectangle (+ -1 tile-width) (+ -1 tile-height) "outline" "red")) 
  
  (underlay/xy  empty-image
               (* frame-x tile-width)
               (* frame-y tile-height)
               frame))

|#

; puts all tiles from list together
(define/memo (mini-map-base tiles columns rows tile-width tile-height total-tiles)
  ;(-> list? integer? integer? integer? integer? integer? image?)
  (define x (modulo (- total-tiles   (length tiles)) columns))
  (define y (quotient (- total-tiles (length tiles)) columns))
  (if (empty? tiles) (rectangle (* tile-width columns)
                                (* tile-height rows) "outline" "black")  
  (freeze (underlay/xy (mini-map-base (rest tiles) columns rows tile-width tile-height total-tiles)
                       (* x tile-width) (* y tile-height)
                       (first tiles)))))

; add frame to mini-map 
(define/contract (frame-mini-map img)
  (-> image? image?)
  (underlay 
    (underlay/align "middle" "middle"
      (rectangle (+ frame-width (image-width img))
                 (+ frame-width (image-height img))
                 "solid" (make-color 190 190 190))
      (rectangle (+ (+ -1 frame-width) (image-width img))
                 (+ (+ -1 frame-width) (image-height img))
                 "solid" (make-color 255 255 255))
      (rectangle (+ (+ -4 frame-width) (image-width img))
                 (+ (+ -4 frame-width) (image-height img))
                 "solid" (make-color 190 190 190)))
  img
  (rectangle (+ frame-width (image-width img))
             (+ frame-width (image-height img))
             "solid" (make-color 198 174 138 80))))

#|
; update mini-map entitiy sprite based on next backdrop tile index for a given direction
(define/contract (update-mini-map-frame layout)
  (-> image? handler-function?)
  (lambda (g e)
    ;(get-component (game->tracking-entity g) backdrop?)
    ;(define tile-index (backdrop-current-tile backdrop))
    (define backdrop     (get-component (game->tracking-entity g) backdrop?))
    (define current-tile (backdrop-current-tile backdrop))
    (if current-tile
        (update-entity e animated-sprite? (new-sprite (mini-map-frame backdrop current-tile layout)))
        e)
    ))

; update mini-map entitiy sprite based on next backdrop tile index for a given direction
(define/contract (update-mini-map-layout)
  (-> handler-function?)
  (lambda (g e)
    (define backdrop     (get-component (game->tracking-entity g) backdrop?))
    (define current-tile (game->current-tile g))
    (if current-tile
        (update-entity e animated-sprite? (new-sprite (mini-map-layout backdrop current-tile)))
        e)
    ))

; returns next tile index in direction
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

; Function for observe-rule change in backdrop id
(define (backdrop-r? g e)
  ;(get-component (game->tracking-entity g) backdrop?)
    ;(define tile-index (backdrop-current-tile backdrop))
  (backdrop-id (get-component (game->tracking-entity g) backdrop?)))
|#