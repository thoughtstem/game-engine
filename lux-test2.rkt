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
<<<<<<< HEAD
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
=======
(define example (make-sheet base GROUND_TILE_SIZE))

(define grass-tile ((sheet-get example) 6 3))
(define grass-sym  'g)

(define (ground-data)
  (grid-of 's WIDTH HEIGHT))

(define (ground)
  (define ret (add-noise 's 0.25
                         (grid-of 'g WIDTH HEIGHT)))
  ret)

(define (sheet->combiner-tiles off-x off-y)
  (flatten (list
            (list (sub-div ((sheet-get example) off-x off-y))       (sub-div ((sheet-get example) (+ 1 off-x) off-y)))
            (list (sub-div ((sheet-get example) off-x (+ 1 off-y))) (sub-div ((sheet-get example) (+ 1 off-x) (+ 1 off-y))))
            (list (sub-div ((sheet-get example) off-x (+ 2 off-y))) (sub-div ((sheet-get example) (+ 1 off-x) (+ 2 off-y)))))))

(define grass-stone-sheet (sheet->combiner-tiles 0 0))

(define big-ground (ground))

(define (paint s x y event)
  (displayln (list x y event))
  (define g (second s))
  (if (string=? event "button-down")
      (list (first s)
            (grid-set g
                (floor (/ y GROUND_TILE_SIZE))
                (floor (/ x GROUND_TILE_SIZE)) 's)
            #f)
      s))

(define (cache-render s)
  (define is (map (curry draw s) '(0 1 2)))
  (define ils (map image:image->color-list is))
  (define f (lambda (il) (image:color-list->bitmap il (image:image-width (first is)) (image:image-height (first is)))))
  (define i2s (map f ils)) ;Making it a bitmap makes rendering much faster...
  (list (first s)
        (second s)
        i2s))
>>>>>>> d8d1f7a7389cdce7199dc29d95295646c7a65257

(define (current-frame time)
  (* 2 (remainder time 3)))

<<<<<<< HEAD
(define (draw s)
  (render (game-terrain-sprite s)))
=======
(define (draw s (extra 0))
  (define g (second s))
  (merge-tiles (render2 g (list-getter (sheet->combiner-tiles (+ 8 (current-frame (+ extra (first s)))) 0)) grass-tile )))
>>>>>>> d8d1f7a7389cdce7199dc29d95295646c7a65257

(struct demo 
  (g/v state)
  #:methods gen:word
  [(define (word-output w)
     (match-define (demo g/v state) w)
<<<<<<< HEAD
     (g/v (draw state)))

   ;Handle events
=======
     (match-define (list time ground img) state)
     (g/v (list-ref img (remainder (floor (/ time 10)) 3))))
>>>>>>> d8d1f7a7389cdce7199dc29d95295646c7a65257
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

