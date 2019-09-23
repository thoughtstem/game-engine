#lang racket

#|(module+ test
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
                  (list 0 0 0))

    (check-equal? (render s)
                  ;(text "Hello" 14 'white)
                  (text "Hello" 13 'white)
                  )

    (check-equal? (render-string s)
                  "Hello")

    (check-equal? (render (increase-current-frame s))
                  (text "Goodbye" 13 'white))

    ;And back to the beginning
    (check-equal? (render (increase-current-frame (increase-current-frame s)))
                  (text "Hello" 13 'white))

    (check-equal? (render-string (set-text "New Text" s))
                  "New Text"))


  ;Example of animating fancier text
  #;(let ()
    (define s (new-sprite (list (make-text-frame "Hello"   #:scale 2 #:color 'red)
                                (make-text-frame "Goodbye" #:scale 2 #:color 'green))))

    (check-equal? (string-animated-sprite? s) #t)

    (check-equal? (render s)
                  (text "Hello" 14 'red))

    (check-equal? (render (increase-current-frame s))
                  (text "Goodbye" 14 'green))

    ;And back to the beginning
    (check-equal? (render (increase-current-frame (increase-current-frame s)))
                  (text "Hello" 14 'red))
    )
  )|#

(provide ;new-sprite
         
         render
         render-string
         render-text-frame

         (except-out (struct-out text-frame) text-frame)
         (rename-out (make-text-frame text-frame))

         set-text-frame-scale
         set-text-frame-font
         set-text-frame-color
         
         next-frame
         set-frame
         sheet->costume-list
         animation-finished?
         reset-animation
         (struct-out animated-sprite)
         sprite?
         animated-sprite-x-scale
         ;sheet->sprite
         sprite->sheet
         ;row->sprite
         sprite-map
         pick-frame
         pick-frame-original
        ; sprite-map-original
         
         fast-equal?
         fast-image-data
         fast-image?
         frame->image
         
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

         get-x-offset
         get-y-offset

         get-rotation
         get-color
         get-sprite-layer

         sprite-width    ; matches syntax of image-width
         sprite-height   ; matches syntax of image-height

         (rename-out (sprite-width get-sprite-width))  ;also providing these for getter consistency
         (rename-out (sprite-height get-sprite-height))
         
         set-x-offset
         set-y-offset
         scale-xy
         set-angle
         set-scale-xy
         set-text
         set-font
         get-font-size

         set-animate?
         
         ;set-sprite-scale
         ;set-sprite-color
         ;set-sprite-angle
         
         change-x-offset
         change-y-offset
         multiply-x-offset
         multiply-y-offset

         string-animated-sprite?
         image-animated-sprite?

         animated-sprite-rgb
         
         freeze-image
         prep-costumes

         MONOSPACE-FONT-FACE)

(require 2htdp/image)
(require threading)
(require (only-in racket/draw
                  the-color-database))
(require "../engine/component-struct.rkt")

(define MONOSPACE-FONT-FACE
  (cond [(eq? (system-type 'os) 'windows) "Consolas" ]
        [(eq? (system-type 'os) 'macosx)  "Menlo"]
        [(eq? (system-type 'os) 'unix)    "DejaVu Sans Mono"]))

;Convenience methods for going from sheets to sprites

#|(define (sheet->sprite sheet #:rows        (r 1)
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
|#

(struct text-frame (string scale font color))

(define (make-text-frame s
                         #:scale [scale 1]
                         #:font [font #f]
                         #:color [color #f])
  (text-frame s scale font color))


(define (set-text-frame-scale s tf)
  (struct-copy text-frame tf
               [scale s]))

(define (set-text-frame-font f tf)
  (struct-copy text-frame tf
               [font f]))

(define (set-text-frame-color c tf)
  (struct-copy text-frame tf
               [color c]))

(define (get-font-size tf)
  (if (text-frame-font tf)
      (send (text-frame-font tf) get-size)
      13))


(struct fast-image (data [id #:mutable]) #:transparent)


;Struct to encapsulate what an animation is
(struct animated-sprite component-struct
        (
         [o-frames #:mutable]        ;List of original images.  This should be fast-images???
         [frames  #:mutable]
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
         layer
         )
  #:transparent
  ;#:mutable
  )

(define sprite? (or/c image? animated-sprite?))


(define/contract (image-animated-sprite? as)
  (-> any/c boolean?)

  ;Is this not right?
  (and (animated-sprite? as)
       (fast-image? (vector-ref (animated-sprite-frames as) 
                                (animated-sprite-current-frame as)))))

(define/contract (sprite->sheet s)
  (-> animated-sprite? image?)
  (define image-list (map frame->image
                          (vector->list
                           (animated-sprite-frames s))))
  (if (= (length image-list) 1)
      (first image-list)
      (apply beside image-list)))


(define/contract (string-animated-sprite? as)
  (-> any/c boolean?)
  (and (animated-sprite? as)
       (text-frame? (vector-ref (animated-sprite-frames as) 
                                (animated-sprite-current-frame as)))))


(define (current-fast-frame as)
  (vector-ref (animated-sprite-frames as)
              (animated-sprite-current-frame as)))

(define (sprite-map f s)
  ;(displayln "Mapping over a sprite.  Slow...  Don't do this at runtime.")
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

(define (set-font f as)

  (define current-text-frame (render-text-frame as))
  ;Does this really need mutation??
  (vector-set! (animated-sprite-frames as)
               (animated-sprite-current-frame as)
               (struct-copy text-frame current-text-frame
                            [font f]))
  
  as)

(define (get-x-scale as)
  (animated-sprite-x-scale as))

(define (get-y-scale as)
  (animated-sprite-y-scale as))

(define (get-x-offset as)
  (animated-sprite-x-offset as))

(define (get-y-offset as)
  (animated-sprite-y-offset as))

(define (get-rotation as)
  (radians->degrees (animated-sprite-rotation as)))

(define (get-color as)
  (animated-sprite-color as))

(define (get-sprite-layer as)
  (animated-sprite-layer as))
 
;
(define/contract (set-x-offset v as)
  (-> number? animated-sprite? animated-sprite?)
  
  ;(set-animated-sprite-x-offset! as v)
  ;as
  (struct-copy animated-sprite as
               [x-offset v])
  )

(define/contract (set-y-offset v as)
  (-> number? animated-sprite? animated-sprite?)
  
  ;(set-animated-sprite-y-offset! as v)
  ;as
  (struct-copy animated-sprite as
               [y-offset v])
  )

(define/contract (set-x-scale s as)
  (-> number? animated-sprite? animated-sprite?)

  ;(set-animated-sprite-x-scale! as (* 1.0 s))
  ;as
  (struct-copy animated-sprite as
               [x-scale (* 1.0 s)])
  )

(define/contract (set-y-scale s as)
  (-> number? animated-sprite? animated-sprite?)
  
  ;(set-animated-sprite-y-scale! as (* 1.0 s))
  ;as
  (struct-copy animated-sprite as
               [y-scale (* 1.0 s)])
  
  )

; sets scale regardles of previous scale
(define/contract (set-scale-xy v as)
  (-> number? animated-sprite? animated-sprite?)
  
  ;(~> as
  ;    (set-x-scale v _)
  ;    (set-y-scale v _))
  ;as
  (struct-copy animated-sprite as
               [x-scale (* 1.0 v)]
               [y-scale (* 1.0 v)])
  )

; multiplies new scale value by previous scale value
(define (scale-xy v as)
  
  ;(set-animated-sprite-x-scale! as (* 1.0 v (animated-sprite-x-scale as)))
  ;(set-animated-sprite-y-scale! as (* 1.0 v (animated-sprite-y-scale as)))
  ;as
  (define old-x-scale (get-x-scale as))
  (define old-y-scale (get-y-scale as))

  (struct-copy animated-sprite as
               [x-scale (* 1.0 v old-x-scale)]
               [y-scale (* 1.0 v old-y-scale)])

  )

(define (set-angle v as)
  ;(set-animated-sprite-rotation! as (* 1.0 (degrees->radians v)))
  ;as
  (struct-copy animated-sprite as
               [rotation (* 1.0 (degrees->radians v))])
  )

(define (set-animate? v as)
  (struct-copy animated-sprite as
               [animate? v])
  )

; === SPRITE MODIFIERS ===
; These are meant to be used at the top level and can take
; either an image or an animated sprite. These also perform
; a struct copy on the sprite. For internal usage, use the
; previous functions for faster performance.

#|(define/contract (set-sprite-scale s as)
  (-> number? (or/c animated-sprite? image?) animated-sprite?)
  
  (define current-x (if (animated-sprite? as)
                        (get-x-scale as)
                        1))
  (define current-y (if (animated-sprite? as)
                        (get-y-scale as)
                        1))
  (if (animated-sprite? as)
      (scale-xy s as)
      (new-sprite as #:scale s)))

(define/contract (set-sprite-color c as)
  (-> symbol? (or/c animated-sprite? image?) animated-sprite?)

  (if (animated-sprite? as)
      (struct-copy animated-sprite as
                   [color c])
      (new-sprite as #:color c))
  )

(define/contract (set-sprite-angle v as)
  (-> number? (or/c animated-sprite? image?) animated-sprite?)

  (if (animated-sprite? as)
      (set-angle v as)
      (new-sprite as #:rotation v))
  )
|#


(define (freeze-image thing)
  (if (image? thing)
      (freeze thing)
      thing))

#;(define/contract (new-sprite costumes (rate 1)
                             #:animate [animate? #t]
                             #:x-offset (x-offset 0)
                             #:y-offset (y-offset 0)
                             #:color    (color 'black)
                             #:scale    (scale #f)
                             #:x-scale  [x-scale 1]
                             #:y-scale  [y-scale 1]
                             #:rotation [deg 0])
  (->* ((or/c image? (listof image?)
              string?     (listof string?)
              text-frame? (listof text-frame?)))
       (number? #:animate boolean?
                #:x-offset number?
                #:y-offset number?
                #:color    symbol?
                #:scale    number?
                #:x-scale  number?
                #:y-scale  number?
                #:rotation number?) animated-sprite?)
  #|(define list-costumes (if (list? costumes)
                            costumes
                            (list costumes)))|#
  ; === TODO: TEST PERFORMANCE OF FREEZING ALL SPRITES ====
  (define list-costumes (if (list? costumes)
                            (map freeze-image costumes)
                            (list (freeze-image costumes))))

  (animated-sprite
   (next-component-id)
   ;Umm we don't need to be storing this two times do we?
   ;JL: This is stored twice to preserve original costumes for functions like
   ;    set-size and set-hue. This can be removed once we have a new way to
   ;    set-hue and all functions in sprite-util are updated.
   (list->vector (map prep-costumes list-costumes)) 
   (list->vector (map prep-costumes list-costumes))
   0
   rate
   0
   animate?
   (if scale scale x-scale)
   (if scale scale y-scale)
   (* 1.0 (degrees->radians deg)) ;theta (in radians)
   x-offset ;x offset
   y-offset ;y offset
   color
   ))

; Is this only for string-animated-sprite?
; What abot when you apply mode-lambda rgb to a sprite?
(define (animated-sprite-rgb as)
  (-> animated-sprite? (listof byte?))

  (define tf-color-symbol (and (string-animated-sprite? as)
                               (text-frame-color (render-text-frame as))))

  (define c (if tf-color-symbol
                (send the-color-database find-color (~a tf-color-symbol))
                (send the-color-database find-color (~a (animated-sprite-color as)))))
  (if c
      (list
       (send c red)
       (send c green)
       (send c blue))
      (list 0 0 0))
      )

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

(define/contract (sprite-width s)
  (-> animated-sprite? number?)
  (* (get-x-scale s) (image-width (pick-frame s
                                            (animated-sprite-current-frame s)))))

(define/contract (sprite-height s)
  (-> animated-sprite? number?)
  (* (get-y-scale s) (image-height (pick-frame s
                                             (animated-sprite-current-frame s)))))

(define/contract (render s)
  (-> animated-sprite? image?)
  (scale/xy
   (abs (animated-sprite-x-scale s)) ;Breaks on negatives...
   (abs (animated-sprite-y-scale s)) ;Breaks on negatives...
   (freeze (pick-frame s
                       (animated-sprite-current-frame s)))
   ))

(define/contract (render-string as)
  (-> string-animated-sprite? string?)

  (text-frame-string
   (vector-ref (animated-sprite-frames as)
               (animated-sprite-current-frame as))))

(define/contract (render-text-frame as)
  (-> string-animated-sprite? text-frame?)

  (vector-ref (animated-sprite-frames as)
              (animated-sprite-current-frame as)))

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
  (define tf-font (text-frame-font thing))
  (scale (text-frame-scale thing)
         (if tf-font
             (text/font (text-frame-string thing)
                        (send tf-font get-size)
                        (if (text-frame-color thing)
                            (text-frame-color thing)
                            'white)
                        (send tf-font get-face)
                        (send tf-font get-family)
                        (send tf-font get-style)
                        (send tf-font get-weight)
                        (send tf-font get-underlined))
             (text/font (text-frame-string thing)
                        13
                        (if (text-frame-color thing)
                            (text-frame-color thing)
                            'white)
                        MONOSPACE-FONT-FACE
                        'modern
                        'normal
                        'normal
                        #f
                        ))))

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

(define/contract (change-x-offset v as)
  (-> number? animated-sprite? animated-sprite?)
  
  ;(set-animated-sprite-x-offset! as v)
  ;as
  (struct-copy animated-sprite as
               [x-offset (+ (get-x-offset as) v)])
  )

(define/contract (change-y-offset v as)
  (-> number? animated-sprite? animated-sprite?)
  
  ;(set-animated-sprite-y-offset! as v)
  ;as
  (struct-copy animated-sprite as
               [y-offset (+ (get-y-offset as) v)])
  )

(define/contract (multiply-x-offset v as)
  (-> number? animated-sprite? animated-sprite?)
  
  ;(set-animated-sprite-x-offset! as v)
  ;as
  (struct-copy animated-sprite as
               [x-offset (* (get-x-offset as) v)])
  )

(define/contract (multiply-y-offset v as)
  (-> number? animated-sprite? animated-sprite?)
  
  ;(set-animated-sprite-y-offset! as v)
  ;as
  (struct-copy animated-sprite as
               [y-offset (* (get-y-offset as) v)])
  )

