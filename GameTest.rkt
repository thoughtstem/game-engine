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


(define (add-grid-indexes g)
  (for/list ([r (range (length g))])
    (for/list ([c (range (length (first g)))])
      (list (grid-get g r c) r c))))

(define (grid-map-with-index f g)
    (grid-map (curry apply f) (add-grid-indexes g)))

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

(define (grass-stone-tiles i)
  (list-ref
   (append
    (sub-div (tile 6 0))
    (sub-div (tile 7 0))
    (sub-div (tile 6 1))
    (sub-div (tile 7 1))
    (sub-div (tile 6 2))
    (sub-div (tile 7 2))) i))

(define (ms pattern)
  (match pattern
    ;Grass on three sides
    ['((g g)
       (g S)) (grass-stone-tiles 0)]    
    ['((g g)
       (S g)) (grass-stone-tiles 1)]
    ['((S g)
       (g g)) (grass-stone-tiles 3)]
    ['((g S)
       (g g)) (grass-stone-tiles 2)]

    ;Grass on two sides
    ['((g g)
       (s S)) (grass-stone-tiles 12)]
    ['((g s)
       (g S)) (grass-stone-tiles 16)]
    ['((s g)
       (g S)) (grass-stone-tiles 0)]

    ;Grass on two sides (rotation 1)
    ['((s g)
       (S g)) (grass-stone-tiles 21)]
    ['((g g)
       (S s)) (grass-stone-tiles 9)]
    ['((g s)
       (S g)) (grass-stone-tiles 1)]

    ;Grass on two sides (rotation 2)
    ['((S s)
       (g g)) (grass-stone-tiles 22)]
    ['((S g)
       (s g)) (grass-stone-tiles 16)]
    ['((S g)
       (g s)) (grass-stone-tiles 3)]

    ;Grass on two sides (rotation 3)
    ['((g S)
       (g s)) (grass-stone-tiles 10)]
    ['((s S)
       (g g)) (grass-stone-tiles 22)]
    ['((g S)
       (s g)) (grass-stone-tiles 3)]

    ;Grass on one side
    ['((s g)
       (s S)) (grass-stone-tiles 12)]
    ['((g s)
       (s S)) (grass-stone-tiles 4)]
    ['((s s)
       (g S)) (grass-stone-tiles 10)]

    ;Grass on one side (rotation 1)
    ['((s s)
       (S g)) (grass-stone-tiles 15)]
    ['((s g)
       (S s)) (grass-stone-tiles 5)]
    ['((g s)
       (S s)) (grass-stone-tiles 9)]

    ;Grass on one side (rotation 2)
    ['((S s)
       (g s)) (grass-stone-tiles 5)]
    ['((S s)
       (s g)) (grass-stone-tiles 9)]
    ['((S g)
       (s s)) (grass-stone-tiles 15)]

    ;Grass on no side
    ['((s s)
       (s S)) (grass-stone-tiles 20)]
    ['((s s)
       (S s)) (grass-stone-tiles 17)]
    ['((S s)
       (s s)) (grass-stone-tiles 11)]
    ['((s S)
       (s s)) (grass-stone-tiles 6)]

    ;Grass on all sides
    ['((g G)
       (g g)) (square 16 "solid" "green")]
    ['((g g)
       (g G)) (square 16 "solid" "green")]
    ['((g g)
       (G g)) (square 16 "solid" "green")]
    ['((G g)
       (g g)) (square 16 "solid" "green")]

    [else (square 16 "solid" "green")]
    ))

(define/contract (grid-get g r c)
  (-> (listof list?) number? number? any/c)
  (define r2 (max r 0))
  (define c2 (max c 0))
  (define r3 (min r2 (- (length g) 1)))
  (define c3 (min c2 (- (length (first g)) 1)))
  (list-ref (list-ref g r3) c3))

(define/contract (grid-get-adj g r c rd cd)
  (-> (listof list?) number? number? number? number? any/c)
  (grid-get g (+ r rd) (+ c cd)))

(define (tl-neighbor g r c)
  (grid-get-adj g r c -1 -1))

(define (t-neighbor g r c)
  (grid-get-adj g r c -1 0))

(define (l-neighbor g r c)
  (grid-get-adj g r c 0 -1))

(define (tr-neighbor g r c)
  (grid-get-adj g r c 1 0))

(define (r-neighbor g r c)
  (grid-get-adj g r c 0 1))

(define (br-neighbor g r c)
  (grid-get-adj g r c 1 1))

(define (b-neighbor g r c)
  (grid-get-adj g r c 1 0))

(define (bl-neighbor g r c)
  (grid-get-adj g r c -1 1))

(define (capitalize sym)
  (string->symbol (string-upcase (symbol->string sym))))

(define/contract (grid->quad g r c dir)
  (-> (listof list?) number? number? symbol? (listof list?))
  (match dir
    ['tl
     (list
      (list (tl-neighbor g r c) (t-neighbor g r c))
      (list (l-neighbor g r c) (capitalize (grid-get g r c))))]
    ['tr
     (list
      (list (t-neighbor g r c) (tr-neighbor g r c))
      (list (capitalize (grid-get g r c)) (r-neighbor g r c) ))]
    ['br
     (list
      (list (capitalize (grid-get g r c)) (r-neighbor g r c))
      (list (b-neighbor g r c) (br-neighbor g r c) ))]
    ['bl
     (list
      (list (l-neighbor g r c) (capitalize (grid-get g r c)) )
      (list (bl-neighbor g r c) (b-neighbor g r c) ))]))

(define/contract (render2-tile g r c)
  (-> (listof list?) number? number? image?)
  (above
   (beside
    (ms (grid->quad g r c 'tl))
    (ms (grid->quad g r c 'tr)))
   (beside
    (ms (grid->quad g r c 'bl))
    (ms (grid->quad g r c 'br)))))

(define (render2 g)
  (for/list ([r (range (length g))])
    (for/list ([c (range (length (first g)))])
      (render2-tile g r c))))

(define small-ground
  '((g g g g)
    (g s g g)
    (g s s s)
    (g g g g)))


(append
    (sub-div (tile 6 0))
    (sub-div (tile 7 0))
    (sub-div (tile 6 1))
    (sub-div (tile 7 1))
    (sub-div (tile 6 2))
    (sub-div (tile 7 2)))

grass-stone-sheet

small-ground

;(add-grid-indexes small-ground)

(render2 small-ground)

(define big-ground (ground))
(render-ground big-ground)

(render2 big-ground)

#;(above
 (beside
  (render2-tile small-ground 0 0)
  (render2-tile small-ground 0 1)
  (render2-tile small-ground 0 2)
  (render2-tile small-ground 0 3))
 (beside
  (render2-tile small-ground 1 0)
  (render2-tile small-ground 1 1)
  (render2-tile small-ground 1 2)
  (render2-tile small-ground 1 3))
 (beside
  (render2-tile small-ground 2 0)
  (render2-tile small-ground 2 1)
  (render2-tile small-ground 2 2)
  (render2-tile small-ground 2 3))
 (beside
  (render2-tile small-ground 3 0)
  (render2-tile small-ground 3 1)
  (render2-tile small-ground 3 2)
  (render2-tile small-ground 3 3)))
