#lang racket

(provide new-sprite
         render
         next-frame
         set-frame
         sheet->costume-list
         animation-finished?
         reset-animation
         (struct-out animated-sprite)
         animated-sprite-x-scale
         sheet->sprite
         row->sprite
         sprite-map
         pick-frame
         pick-frame-original
        ; sprite-map-original
         
         fast-equal?
         fast-image-data
         fast-image?
         
         finalize-fast-image
         (rename-out [get-fast-image-id fast-image-id])
         (rename-out [make-fast-image fast-image])
         animated-sprite-total-frames

         current-fast-frame
         get-image-id
         set-x-scale
         set-y-scale
         set-x-offset
         set-y-offset
         scale-xy
         set-angle
         set-scale-xy)

(require 2htdp/image)
(require threading)

;Convenience methods for going from sheets to sprites

(define (sheet->sprite sheet #:rows        (r 1)
                             #:columns     (c 1)
                             #:row-number  (n 1)
                             #:speed       (speed #f)
                             #:delay       (delay #f)
                             #:animate?     [animate? #t])
  
  (define actual-delay (or delay speed 1))
  
  (~> sheet
      (sheet->costume-list _ c r (* r c))
      (drop _ (* (- n 1) c))
      (take _ c)
      (new-sprite _ actual-delay #:animate animate?)))

(define (row->sprite sheet
                     #:columns     (c 4)
                     #:row-number  (n 1)
                     #:delay       (delay 1))
  
  (sheet->sprite sheet
                 #:rows 1
                 #:columns c
                 #:row-number n
                 #:delay delay))


;Struct to encapsulate what an animation is
(struct animated-sprite
        (
         o-frames         ;List of original images.  This should be fast-images???
         frames
         current-frame    ;Frame to show currently (integer)
         rate             ;How many ticks before switching frames (integer)
         ticks            ;How many ticks have passed since last frame change (integer)
         animate?         ;Set true to animate frames
         x-scale
         y-scale
         rotation         ;radians
         x-offset
         y-offset
         )
  #:transparent)

(define (current-fast-frame as)
  (vector-ref (animated-sprite-frames as)
              (animated-sprite-current-frame as)))

(define (sprite-map f s)
  (displayln "Mapping over a sprite.  Slow...  Don't do this at runtime.")
  (define new-frames (vector-map (compose f fast-image-data) (animated-sprite-frames s)))
  
  (struct-copy animated-sprite s
               [frames (vector-map make-fast-image new-frames)]))


(define/contract (set-x-offset v as)
  (-> number? animated-sprite? animated-sprite?)
  
  (struct-copy animated-sprite as
               [x-offset v]))

(define/contract (set-y-offset v as)
  (-> number? animated-sprite? animated-sprite?)
  
  (struct-copy animated-sprite as
               [y-offset v]))



(define/contract (set-x-scale s as)
  (-> number? animated-sprite? animated-sprite?)
  
  (struct-copy animated-sprite as
               [x-scale (* 1.0 s)]))

(define/contract (set-y-scale s as)
  (-> number? animated-sprite? animated-sprite?)
  
  (struct-copy animated-sprite as
               [y-scale (* 1.0 s)]))

(define/contract (set-scale-xy v as)
  (-> number? animated-sprite? animated-sprite?)
  
  (~> as
      (set-x-scale v _)
      (set-y-scale v _)))

(define (scale-xy v as)
  (struct-copy animated-sprite as
               [x-scale (* 1.0 v (animated-sprite-x-scale as))]
               [y-scale (* 1.0 v (animated-sprite-y-scale as))]))

(define (set-angle v as)
  (struct-copy animated-sprite as
               [rotation (* 1.0 (degrees->radians v))]))


(define/contract (new-sprite costumes (rate 1) #:animate [animate? #t])
  (->* ((or/c image? (listof image?))) (number? #:animate boolean?) animated-sprite?)
  (define list-costumes (if (list? costumes)
                            costumes
                            (list costumes)))

  (animated-sprite
   ;Umm we don't need to be storing this three times...
   (list->vector (map make-fast-image list-costumes))
   (list->vector (map make-fast-image list-costumes))
   0
   rate
   0
   animate?
   1.0 ;x-scale
   1.0 ;y-scale
   0.0 ;theta (in radians)
   0.0 ;x offset
   0.0 ;y offset
   ))


(define (animated-sprite-total-frames s)
  (vector-length (animated-sprite-frames s)))

(define/contract (animation-finished? s)
  (-> animated-sprite? boolean?)
  (= (sub1 (animated-sprite-total-frames s)) (animated-sprite-current-frame s)))

(define/contract (render s)
  (-> animated-sprite? image?)

  (scale/xy
   (max 1 (animated-sprite-x-scale s)) ;Breaks on negatives...
   (max 1 (animated-sprite-y-scale s)) ;Breaks on negatives...
   (pick-frame s
               (animated-sprite-current-frame s)))

  )

(define/contract (pick-frame s i)
  (-> animated-sprite? integer? image?)
  (fast-image-data (vector-ref (animated-sprite-frames s) i)))

(define/contract (pick-frame-original s i)
  (-> animated-sprite? integer? image?)
  (fast-image-data (vector-ref (animated-sprite-o-frames s) i)))

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

(define/contract (set-frame s i)
  (-> animated-sprite? number? animated-sprite?)
  (struct-copy animated-sprite s
               [current-frame i]))

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


(struct fast-image (data [id #:mutable]) #:transparent)

(define (get-fast-image-id fi)
  (if (procedure? (fast-image-id fi))
      (fast-image-id (finalize-fast-image fi))
      (fast-image-id fi)))

(define (finalize-fast-image fi)
  (displayln (~a "Finalizing a fast image sized: " (image-width (fast-image-data fi)) "x" (image-height (fast-image-data fi))))
  
  (set-fast-image-id! fi ((fast-image-id fi)))
  (displayln "Done finalizing fast image:")

  (displayln fi)
  
  fi)

(define (fast-equal? i1 i2)
  (equal? (get-fast-image-id i1)
          (get-fast-image-id i2)))

(define (make-fast-image i)
  (displayln (~a "Making fast image for image sized: " (image-width i) "x" (image-height i)))
  
  (define ret
    (if (fast-image? i)
        i
        (begin
          (fast-image i (thunk (get-image-id i)) ))))

  ret)

(define (get-image-id i)
  (equal-hash-code (~a (image->color-list i))))
