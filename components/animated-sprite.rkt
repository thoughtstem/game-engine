#lang racket

(provide new-sprite
         render
         next-frame
         sheet->costume-list
         animation-finished?
         reset-animation
         (struct-out animated-sprite)
         sheet->sprite
         sprite-map)


(require 2htdp/image)
(require threading)

;Convenience methods for going from sheets to sprites

(define (sheet->sprite sheet #:rows    r
                             #:columns c
                             #:row-number  n
                             #:speed   s)
  (~> sheet
      (sheet->costume-list _ c r (* r c))
      (drop _ (* (- n 1) c))
      (take _ c)
      (new-sprite _ s)))  


(struct animated-sprite
        (
         frames           ;List of images
         total-frames
         current-frame    ;Frame to show currently (integer)
         rate             ;How many ticks before switching frames (integer)
         ticks            ;How many ticks have passed since last frame change (integer)
         ) #:transparent)

(define (sprite-map f s)
  (struct-copy animated-sprite s
               [frames (vector-map f (animated-sprite-frames s))]))

(define/contract (new-sprite costumes (rate 1))
  (->* ((or/c image? (listof image?))) (number?) animated-sprite?)
  (define list-costumes (if (list? costumes)
                            costumes
                            (list costumes)))
  (animated-sprite
    (list->vector (map bake list-costumes))
    (length list-costumes)
    0
    rate
    0))

(define (bake i)
  (define w (image-width i))
  (define h (image-height i))
  (color-list->bitmap
   (image->color-list i)
   w
   h))

(define/contract (animation-finished? s)
  (-> animated-sprite? boolean?)
  (= (sub1 (animated-sprite-total-frames s)) (animated-sprite-current-frame s)))

(define/contract (render s)
  (-> animated-sprite? image?)
  (pick-frame s
              (animated-sprite-current-frame s)))

(define/contract (pick-frame s i)
  (-> animated-sprite? integer? image?)
  (vector-ref (animated-sprite-frames s) i))

(define/contract (reset-animation s)
  (-> animated-sprite? animated-sprite?)
  (struct-copy animated-sprite s
               [ticks 0]
               [current-frame 0]))

(define/contract (next-frame s)
  (-> animated-sprite? animated-sprite?)
  (if (= (animated-sprite-ticks s) (animated-sprite-rate s))
      (increase-current-frame s)
      (increase-ticks s)))

(define/contract (increase-ticks s)
  (-> animated-sprite? animated-sprite?)
  (struct-copy animated-sprite s
               [ticks (+ (animated-sprite-ticks s) 1)]))

(define/contract (increase-current-frame s)
  (-> animated-sprite? animated-sprite?)
  (struct-copy animated-sprite s
               [current-frame
                (inc-wrap (animated-sprite-current-frame s)
                          (animated-sprite-total-frames s))]
               [ticks 0]))

(define (inc-wrap n max)
  (remainder (+ 1 n) max))


(define (sheet->costume x y img tiles-across tiles-down)
  (define tile-width (/ (image-width img) tiles-across))
  (define tile-height (/ (image-height img) tiles-down))
  (crop (* x tile-width)
        (* y tile-height)
        tile-width
        tile-height
        img))

(define (sheet->costume-grid sheet tiles-across tiles-down)
  (for/list ([y (range tiles-down)])
    (for/list ([x (range tiles-across)])
      (sheet->costume x y sheet tiles-across tiles-down))))

(define (sheet->costume-list sheet tiles-across tiles-down total)
  (take (flatten (sheet->costume-grid sheet tiles-across tiles-down))
        total))





