#lang racket

;Okay this is much faster (probably b/c of bitmap optimization, though -- not lux per se).
;And the interactions are more full-featured than 2htdp.
;But my code is getting messy.
;Also, need to bring back water animations (enhance the cache-render baking process...)
;Need to support multiple ground types (not just two).  Some way of declaring symbol->tile mappings

(require racket/match
         racket/fixnum
         racket/gui/base
         racket/class
         (prefix-in image: 2htdp/image)
         lux
         lux/chaos/gui
         lux/chaos/gui/val
         lux/chaos/gui/key
         lux/chaos/gui/mouse)

(require "grids.rkt")
(require "terrain-data.rkt")
(require "sheets.rkt")
(require "marching-squares.rkt")
(require "animated-sprites.rkt")

(define WIDTH  10)
(define HEIGHT 10)
(define GROUND_TILE_SIZE 32)

(define base (image:bitmap "./ground-tiles.png"))

(define terrain-sheet (make-sheet base GROUND_TILE_SIZE))
(define grass-tile ((sheet-get terrain-sheet) 6 3))

(define (ground) (ground-gen-uniform2 WIDTH HEIGHT 'g 's 0.25))

(struct game
  (terrain-data
   terrain-sprite)
  #:transparent)

(define (draw-ground g n)
  (bake
   (merge-tiles
    (render2 g (list-getter (sheet->combiner-tiles terrain-sheet (* 2 n) 0)) grass-tile ))))


(define (bake i)
  (define il (image:image->color-list i))
  (define i2 (image:color-list->bitmap il (image:image-width i) (image:image-height i))) ;Making it a bitmap makes rendering much faster...
  i2)

(define (ground->sprite data)
  (new-sprite
          (map (curry draw-ground data) '(0 1 2))
          7))

(define (initial)
  (define initial-ground
    (ground))
  (game initial-ground
        (ground->sprite initial-ground)))



(define (paint s x y)
  (match-define (game data sprite) s)
  (define new-ground (grid-set data
                               (floor (/ y GROUND_TILE_SIZE))
                               (floor (/ x GROUND_TILE_SIZE)) 's))
  (game
       new-ground
       (ground->sprite new-ground)))


(define (current-frame time)
  (* 2 (remainder time 3)))


(define (draw s)
  (render (game-terrain-sprite s)))

(struct demo 
  (g/v state)
  #:methods gen:word
  [(define (word-output w)
     (match-define (demo g/v state) w)
     (g/v (draw state)))

   (define (word-event w e)
     (match-define (demo g/v state) w)
     (match-define (game g sprite) state)
     (define closed? #f)
     (cond
       [(eq? e 'close) #f]
       
       ;If any key is pressed, generate a new terrain
       [(and (key-event? e)
             (not (eq? 'release (send e get-key-code))))
        (let [(new-ground (ground))]
          (demo g/v (initial)))]

       ;On click, draw a new stone/water/whatever
       [(and (mouse-event? e) (send e button-down?))
        (let-values [((x y) (mouse-event-xy e))]
          (let [(new-state (paint state x y))]
            (demo g/v new-state)))]
       
       [else
        (demo g/v state)]))

   (define (word-tick w)
     (match-define (demo g/v state) w)
     (match-define (game data sprite) state)
     (demo g/v (game data (next-frame sprite))))])



(module+ main
  (call-with-chaos
   (make-gui #:width  (* GROUND_TILE_SIZE WIDTH)	 	 	 	 
             #:height (* GROUND_TILE_SIZE HEIGHT))
   (λ () (fiat-lux (demo (make-gui/val) (initial))))))

