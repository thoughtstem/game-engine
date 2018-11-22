#lang racket

(provide new-sprite
         render
         next-frame
         sheet->costume-list
         animation-finished?
         reset-animation
         (struct-out animated-sprite)
         sheet->sprite
         sprite-map
         pick-frame
         pick-frame-original
         sprite-map-original
         animated-sprite-image-eq?)

(require 2htdp/image)
(require threading)

;Convenience methods for going from sheets to sprites

(define (sheet->sprite sheet #:rows        (r 1)
                             #:columns     (c 1)
                             #:row-number  (n 1)
                             #:speed       (speed #f)
                             #:delay       (delay #f)
                             )
  
  (define actual-delay (or delay speed 1))
  
  (~> sheet
      (sheet->costume-list _ c r (* r c))
      (drop _ (* (- n 1) c))
      (take _ c)
      (new-sprite _ actual-delay)))




(struct animated-sprite
        (
         image-id         ;Only changes when the image changes, makes for fast image comparisions, animated-sprite-image-eq?
         
         o-frames         ;List of original images
         frames           ;List of images
         total-frames
         current-frame    ;Frame to show currently (integer)
         rate             ;How many ticks before switching frames (integer)
         ticks            ;How many ticks have passed since last frame change (integer)
         animate?         ;Set true to animate frames
         ) #:transparent)

(define sprite-ids (make-weak-hash))
(define (id-for image-vector)
  (if (hash-has-key? sprite-ids image-vector)
      (hash-ref sprite-ids image-vector)
      (begin
        (hash-set! sprite-ids image-vector (length (hash-keys sprite-ids)))
        (hash-ref  sprite-ids image-vector))))

(define (animated-sprite-image-eq? s1 s2)
  (= (animated-sprite-image-id s1)
     (animated-sprite-image-id s2)))

(define (sprite-map f s)
  (define new-frames (vector-map f (animated-sprite-frames s)))
  
  (struct-copy animated-sprite s
               [image-id (id-for new-frames)]
               [frames new-frames]))

(define (sprite-map-original f s)
  (define new-frames (vector-map f (animated-sprite-o-frames s)))
  (struct-copy animated-sprite s
               [image-id (id-for new-frames)]
               [frames new-frames]))

(define/contract (new-sprite costumes (rate 1) #:animate [animate? #t])
  (->* ((or/c image? (listof image?))) (number?) animated-sprite?)
  (define list-costumes (if (list? costumes)
                            costumes
                            (list costumes)))

  (define frames (map bake list-costumes))
  
  (animated-sprite
   (id-for frames)
   (list->vector frames)
   (list->vector frames)
   (length list-costumes)
   0
   rate
   0
   animate?))

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

(define/contract (pick-frame-original s i)
  (-> animated-sprite? integer? image?)
  (vector-ref (animated-sprite-o-frames s) i))

(define/contract (reset-animation s)
  (-> animated-sprite? animated-sprite?)
  (struct-copy animated-sprite s
               [ticks 0]
               [current-frame 0]))

(define/contract (next-frame s)
  (-> animated-sprite? animated-sprite?)
  (if (animated-sprite-animate? s)
      (if (= (animated-sprite-ticks s) (animated-sprite-rate s))
          (increase-current-frame s)
          (increase-ticks s))
      s))

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

