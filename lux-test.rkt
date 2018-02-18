#lang racket

;Okay this is much faster (probably b/c of bitmap optimization, though -- not lux per se).
;And the interactions are more full-featured than 2htdp.
;But my code is getting messy.
;Also, need to bring back water animations (enhance the cache-render baking process...)

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
(require "sheets.rkt")
(require "marching-squares.rkt")

(define WIDTH  10)
(define HEIGHT 10)
(define GROUND_TILE_SIZE 32)

(define base (image:bitmap "./ground-tiles.png"))
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

(define (current-frame time)
  (* 2 (remainder time 3)))

(define (draw s (extra 0))
  (define g (second s))
  (merge-tiles (render2 g (list-getter (sheet->combiner-tiles (+ 8 (current-frame (+ extra (first s)))) 0)) grass-tile )))

(struct demo 
  (g/v state)
  #:methods gen:word
  [(define (word-output w)
     (match-define (demo g/v state) w)
     (match-define (list time ground img) state)
     (g/v (list-ref img (remainder (floor (/ time 10)) 3))))
   (define (word-event w e)
     (match-define (demo g/v state) w)
     (match-define (list time g img) state)
     (define closed? #f)
     (cond
       [(eq? e 'close)
        #f]
       [(and (key-event? e)
             (not (eq? 'release (send e get-key-code))))
        (let [(new-ground (ground))]
          (demo g/v (cache-render (list 0 new-ground #f))))]
       [(and (mouse-event? e) (send e button-down?))
        (let-values [((x y) (mouse-event-xy e))]
          (let [(new-state (paint state x y "button-down"))]
            (demo g/v (cache-render new-state))))]
       [else
        (demo g/v state)]))
   (define (word-tick w)
     (match-define (demo g/v state) w)
     (match-define (list time data img) state)
     (demo g/v (list (add1 time) data img)))])

(define initial
  (list 0 big-ground #f))

(define initial-cached
  (cache-render initial))

(module+ main
  (call-with-chaos
   (make-gui #:width (* GROUND_TILE_SIZE WIDTH)	 	 	 	 
             #:height (* GROUND_TILE_SIZE HEIGHT))
   (λ () (fiat-lux (demo (make-gui/val) (cache-render initial))))))


