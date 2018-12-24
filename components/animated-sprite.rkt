#lang racket

(module+ test
  (require rackunit)


  ;Example of animating images
  (let ()
    (define s (new-sprite (list (circle 10 'solid 'red)
                                (circle 10 'solid 'green))))

    (check-equal? (image-animated-sprite? s) #t)
    
    (check-equal? (render s)
                  (circle 10 'solid 'red))

    (check-equal? (render (increase-current-frame s))
                  (circle 10 'solid 'green))

    ;And back to the beginning
    (check-equal? (render (increase-current-frame (increase-current-frame s)))
                  (circle 10 'solid 'red)))


  ;Example of animating text
  (let ()
    (define s (new-sprite (list "Hello"
                                "Goodbye")))

    (check-equal? (string-animated-sprite? s) #t)

    (check-equal? (animated-sprite-rgb s)
                  (list 255 255 255))

    (check-equal? (render s)
                  (text "Hello" 14 'white))

    (check-equal? (render-string s)
                  "Hello")

    (check-equal? (render (increase-current-frame s))
                  (text "Goodbye" 14 'white))

    ;And back to the beginning
    (check-equal? (render (increase-current-frame (increase-current-frame s)))
                  (text "Hello" 14 'white))

    (check-equal? (render-string (set-text "New Text" s))
                  "New Text"))


  ;Example of animating fancier text
  ;  (NOT yet implemented in rendering.rkt.)
  (let ()
    (define s (new-sprite (list (text-frame "Hello"   14 'red)
                                (text-frame "Goodbye" 14 'green))))

    (check-equal? (string-animated-sprite? s) #t)

    (check-equal? (render s)
                  (text "Hello" 14 'red))

    (check-equal? (render (increase-current-frame s))
                  (text "Goodbye" 14 'green))

    ;And back to the beginning
    (check-equal? (render (increase-current-frame (increase-current-frame s)))
                  (text "Hello" 14 'red))
    )
  )

(provide new-sprite
         
         render
         render-string
         
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
         
         get-x-scale
         get-y-scale
         
         set-x-offset
         set-y-offset
         scale-xy
         set-angle
         set-scale-xy
         set-text

         string-animated-sprite?
         image-animated-sprite?

         animated-sprite-rgb)

(require 2htdp/image)
(require threading)
(require (only-in racket/draw
                  the-color-database))

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
      (new-sprite _ actual-delay #:animate animate?)
      ))

(define (row->sprite sheet
                     #:columns     (c 4)
                     #:row-number  (n 1)
                     #:delay       (delay 1))
  
  (sheet->sprite sheet
                 #:rows 1
                 #:columns c
                 #:row-number n
                 #:delay delay))

(struct text-frame (string size color))

(define (make-text-frame s)
  (text-frame s 14 'white))


(struct fast-image (data [id #:mutable]) #:transparent)


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
         color
         )
  #:transparent
  #:mutable)



(define/contract (image-animated-sprite? as)
  (-> any/c boolean?)

  ;Is this not right?
  (and (animated-sprite? as)
       (fast-image? (vector-ref (animated-sprite-frames as) 
                                (animated-sprite-current-frame as)))))


(define/contract (string-animated-sprite? as)
  (-> any/c boolean?)
  (and (animated-sprite? as)
       (text-frame? (vector-ref (animated-sprite-frames as) 
                                (animated-sprite-current-frame as)))))


(define (current-fast-frame as)
  (vector-ref (animated-sprite-frames as)
              (animated-sprite-current-frame as)))

(define (sprite-map f s)
  (displayln "Mapping over a sprite.  Slow...  Don't do this at runtime.")
  (define new-frames (vector-map (compose f fast-image-data) (animated-sprite-frames s)))
  
  (struct-copy animated-sprite s
               [frames (vector-map make-fast-image new-frames)]))


(define/contract (set-text v as)
  (-> string? string-animated-sprite? string-animated-sprite?)

  (define f (vector-ref (animated-sprite-frames as)
                        (animated-sprite-current-frame as)))


  ;Does this really need mutation??
  (vector-set! (animated-sprite-frames as)
               (animated-sprite-current-frame as)
               (struct-copy text-frame f
                            [string v]))
  
  as)

(define (get-x-scale as)
  (animated-sprite-x-scale as))

(define (get-y-scale as)
  (animated-sprite-y-scale as))
 

(define/contract (set-x-offset v as)
  (-> number? animated-sprite? animated-sprite?)
  
  (set-animated-sprite-x-offset! as v)
  as)

(define/contract (set-y-offset v as)
  (-> number? animated-sprite? animated-sprite?)
  
  (set-animated-sprite-y-offset! as v)
  as)



(define/contract (set-x-scale s as)
  (-> number? animated-sprite? animated-sprite?)

  (set-animated-sprite-x-scale! as (* 1.0 s))
  as)

(define/contract (set-y-scale s as)
  (-> number? animated-sprite? animated-sprite?)
  
  (set-animated-sprite-y-scale! as (* 1.0 s))
  as)

(define/contract (set-scale-xy v as)
  (-> number? animated-sprite? animated-sprite?)
  
  (~> as
      (set-x-scale v _)
      (set-y-scale v _))
  as)

(define (scale-xy v as)

  (set-animated-sprite-x-scale! as (* 1.0 v (animated-sprite-x-scale as)))
  (set-animated-sprite-y-scale! as (* 1.0 v (animated-sprite-y-scale as)))
  
  as)

(define (set-angle v as)
  (set-animated-sprite-rotation! as (* 1.0 (degrees->radians v)))
  as)


(define/contract (new-sprite costumes (rate 1)
                             #:animate [animate? #t]
                             #:x-offset (x-offset 0)
                             #:y-offset (y-offset 0)
                             #:color    (color 'white)
                             #:scale    (scale 1))
  (->* ((or/c image? (listof image?)
              string?     (listof string?)
              text-frame? (listof text-frame?)))
       (number? #:animate boolean?
                #:x-offset number?
                #:y-offset number?
                #:color    symbol?
                #:scale    number?) animated-sprite?)
  (define list-costumes (if (list? costumes)
                            costumes
                            (list costumes)))

  (animated-sprite
   ;Umm we don't need to be storing this two times do we?
   (list->vector (map prep-costumes list-costumes)) 
   (list->vector (map prep-costumes list-costumes))
   0
   rate
   0
   animate?
   scale ;x-scale
   scale ;y-scale
   0.0 ;theta (in radians)
   x-offset ;x offset
   y-offset ;y offset
   color
   ))

(define (animated-sprite-rgb as)
  (-> animated-sprite? (listof byte?))

  (define c (send the-color-database find-color (~a (animated-sprite-color as))))

  (list
   (send c red)
   (send c green)
   (send c blue)))

(define (prep-costumes thing)
  (cond [(image? thing)  (make-fast-image thing)]
        [(string? thing) (make-text-frame thing)]
        [(text-frame? thing) thing]
        [else (error "What is this?")]))


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
               (animated-sprite-current-frame s))
   ))


(define/contract (render-string as)
  (-> string-animated-sprite? string?)

  (text-frame-string
   (vector-ref (animated-sprite-frames as)
               (animated-sprite-current-frame as))))

(define/contract (pick-frame s i)
  (-> animated-sprite? integer? image?)
  (frame->image (vector-ref (animated-sprite-frames s) i)))

(define/contract (pick-frame-original s i)
  (-> animated-sprite? integer? image?)
  (frame->image (vector-ref (animated-sprite-o-frames s) i)))

(define/contract (frame->image thing)
  (-> (or/c fast-image? text-frame?) image?)
  (cond [(fast-image? thing)  (fast-image-data thing)]
        [(text-frame? thing)  (text-frame->image thing)]
        [else (error "What is this?")]))

(define (text-frame->image thing)
  (text (text-frame-string thing)
        (text-frame-size thing)
        (text-frame-color thing)))

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


(define (get-fast-image-id fi)
  (cond [(not (fast-image? fi)) #f]
        [(procedure? (fast-image-id fi))
         (fast-image-id (finalize-fast-image fi))]
        [else (fast-image-id fi)]))

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
  ;(displayln (~a "Making fast image for image sized: " (image-width i) "x" (image-height i)))
  
  (define ret
    (if (fast-image? i)
        i
        (begin
          (fast-image i (thunk (get-image-id i)) ))))

  ret)

(define (get-image-id i)
  (equal-hash-code (~a (image->color-list i))))


