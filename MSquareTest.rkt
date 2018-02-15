#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(require threading)


(define WIDTH 10)
(define HEIGHT 10)

(define GROUND_TILES (bitmap "./ground-tiles.png"))

(define GROUND_TILE_SIZE 32)


(define (tile x y)
  (crop (* x GROUND_TILE_SIZE)
        (* y GROUND_TILE_SIZE)
        GROUND_TILE_SIZE
        GROUND_TILE_SIZE
        GROUND_TILES))

(define grass-tile (tile 6 3))
(define grass-sym  'g)

(define stone-tile (tile 6 0))
(define stone-sym  's)

(define (list-of t n)
  (map (thunk* t) (range n)))

(define (grid-of t n m)
  (define row (list-of t n))
  (list-of row m))


(define (ground-data)
  (grid-of 's WIDTH HEIGHT))


(define (grid-map f g)
  (define rf (curry map f))
  (map rf g))

(define (choose-between prob a b)
  (if (< (random) prob)
      a
      b))

(define (add-noise sym g)
  (grid-map (curry choose-between 0.25 sym) g))



;TODO: Make into macro
(define (mapping sym)
  (cond 
    [(eq? sym stone-sym) stone-tile]
    [(eq? sym grass-sym) grass-tile]))

(define (render-ground g)
  (define row->tiles (curry map mapping))
  (define tile-grid (map row->tiles g))
  (define render-row (curry apply beside))
  (apply above (map render-row tile-grid)))

(define (solid-ground sym)
  (render-ground
   (grid-of sym WIDTH HEIGHT)))

(define (ground)
  (add-noise 's
             (grid-of 'g WIDTH HEIGHT)))

(render-ground (ground))
(ground)

(define (half-width i)
  (/ (image-width i) 2))

(define (corner x y i)
  (define hw (half-width i))
  (crop (* x hw) (* y hw) 
        hw
        hw
        i))

(define (tl-corner i)
  (corner 0 0 i))

(define (tr-corner i)
  (corner 1 0 i))

(define (bl-corner i)
  (corner 0 1 i))

(define (br-corner i)
  (corner 1 1 i))

;Marching squares:
(define (sub-div i)
  (list (tl-corner i)
        (tr-corner i)
        (bl-corner i)
        (br-corner i)))

(define (f i)
  (overlay
   (square (image-width i) 
           "outline"
           "black")
   i))

(define (show-corners i)
  (define corners (sub-div i))
  (define tl (first corners))
  (define tr (second corners))
  (define bl (third corners))
  (define br (fourth corners))
  (above
   (beside (f tl) (f tr))
   (beside (f bl) (f br))))

(define grass-stone-sheet
  (above
   (beside (show-corners (tile 6 0)) (show-corners (tile 7 0)))
   (beside (show-corners (tile 6 1)) (show-corners (tile 7 1)))
   (beside (show-corners (tile 6 2)) (show-corners (tile 7 2)))))

grass-stone-sheet

(define (test i)
  (define l
    (append
     (sub-div (tile 6 0))
     (sub-div (tile 7 0))
     (sub-div (tile 6 1))
     (sub-div (tile 7 1))
     (sub-div (tile 6 2))
     (sub-div (tile 7 2))))
  (list-ref l i))


(define stone-1
  (tile 1)
  )


