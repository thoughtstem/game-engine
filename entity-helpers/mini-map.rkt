#lang racket

(provide open-mini-map)

(require "../game-entities.rkt"
         "../component-util.rkt"
         "../components/animated-sprite.rkt"
         "../components/detect-edge.rkt"
         "../components/on-edge.rkt"
         "../components/on-rule.rkt"
         "../components/backdrop.rkt"
         "../components/on-start.rkt"
         "../components/spawn-once.rkt"
         "../components/on-key.rkt"
         "../components/after-time.rkt"
         "../entity-helpers/sprite-util.rkt"
         "../entity-helpers/movement-util.rkt"
         2htdp/image
         posn)

(define handler-function? (-> game? entity? entity?))
(define rule?             (-> game? entity? boolean?))

; allows to add mini-map entity to the game.
; Requires game to have an entity with a backdrop component.
; Can be called from any entity: (on-key "m" (open-mini-map #:close-key 'o))
(define/contract (open-mini-map #:close-key close-key)
  (-> #:close-key (or/c symbol? string?) handler-function?)
  (lambda (g e)
    (define backdrop          (get-component (game->tracking-entity g) backdrop?))
    (define tile-index        (backdrop-current-tile backdrop))
    (define mini-map          (mini-map-img backdrop tile-index))
    (define mini-map-offset-x (* -0.05 (game-width g)))
    (define mini-map-offset-y (* -0.05 (game-height g)))
    
    (define mini-map-entity
      (sprite->entity mini-map
                      #:name       "mini-map"
                      #:position   (posn 0 0)
                      #:components (hidden)
                                   (static)
                                   (on-start (do-many (go-to-pos-inside 'bottom-right)
                                                      (change-x-by mini-map-offset-x)
                                                      (change-y-by mini-map-offset-y)
                                                      show))
                                   (on-key close-key die)
                                   (on-rule tile-changed? (update-mini-map))))
    (if (get-entity "mini-map" g)
        e
        (add-components e (spawn-once mini-map-entity #:relative? #f)))))

; create an image for a mini-map entity animated-sprite
(define/contract (mini-map-img backdrop tile-index)
  (-> backdrop? integer? image?)
  (define mini-map-scale 0.07)
  
  (define tiles      (backdrop-tiles backdrop))
  (define columns    (backdrop-columns backdrop))
  (define rows       (/ (length tiles) columns))

  (define tile-width  (image-width  (first tiles)))
  (define tile-height (image-height (first tiles)))

  (define frame-x (modulo   tile-index columns))
  (define frame-y (quotient tile-index columns))
  
  (define frame (rectangle tile-width tile-height "outline" "red")) 
  (define mini-map (mini-map-layout tiles
                                    columns rows
                                    tile-width tile-height
                                    (length tiles)))

  (frame-mini-map (scale mini-map-scale (underlay/xy mini-map
                                        (* frame-x tile-width)
                                        (* frame-y tile-height)
                                        frame))))

; puts all tiles from list together
(define/contract (mini-map-layout tiles columns rows tile-width tile-height total-tiles)
  (-> list? integer? integer? integer? integer? integer? image?)
  (define x (modulo (- total-tiles   (length tiles)) columns))
  (define y (quotient (- total-tiles (length tiles)) columns))
  (if (empty? tiles) (rectangle (* tile-width columns)
                                (* tile-height rows) "outline" "black")  
  (freeze (underlay/xy (mini-map-layout (rest tiles) columns rows tile-width tile-height total-tiles)
                       (* x tile-width) (* y tile-height)
                       (first tiles)))))

; add frame to mini-map 
(define/contract (frame-mini-map img)
  (-> image? image?)
  (define frame-width 6)
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

; update mini-map entitiy sprite based on next backdrop tile index for a given direction
(define/contract (update-mini-map)
  (-> handler-function?)
  (lambda (g e)
    (define backdrop     (get-component (game->tracking-entity g) backdrop?))
    (define current-tile (game->current-tile g))
    (if current-tile
        (update-entity e animated-sprite? (new-sprite (mini-map-img backdrop current-tile)))
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
