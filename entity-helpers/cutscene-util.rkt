#lang racket

(provide cutscene
         change-cutscene
         change-cutscene-by
         page
         move-a-sprite
         scale-a-sprite
         motion-item
         ;images-or-images?
         ;string-or-text-frame?
         
         scroll-right
         scroll-left
         scroll-up
         scroll-down
         scroll-right-from-center
         scroll-left-from-center
         scroll-down-from-center
         scroll-up-from-center

         fade-in
         fade-out
         )

(require "../game-entities.rkt"
         "../component-util.rkt"
         "../components/animated-sprite.rkt"
         "../components/direction.rkt"
         "../components/speed.rkt"
         "../components/on-start.rkt"
         "../components/every-tick.rkt"
         "../components/after-time.rkt"
         "../components/storage.rkt"
         "../components/on-key.rkt"
         "../components/counter.rkt"
         ;"../components/backpack.rkt"
         "../ai.rkt"
         "./sprite-util.rkt"
         "./movement-util.rkt"
         "./text-util.rkt"
         "./ui-util.rkt")

(require 2htdp/image
         (only-in pict
                  filled-rounded-rectangle
                  inset
                  pict->bitmap)
         posn
         threading)

(define image-or-images?
  (or/c image? (listof image?)))

(define string-or-text-frame?
      (or/c image? (listof image?)
            string?     (listof string?)
            text-frame? (listof text-frame?)))


; ==== SPRITE MOTION FUNCTIONS ====

(define (move-a-sprite as #:direction [dir 0] #:speed [spd 0])
  (lambda (g e)
    (define current-as (get-component e (curry component-eq? as)))
    (if current-as
        (let ([updated-sprite (move-sprite current-as
                                           #:direction dir
                                           #:speed spd)])
          (update-entity e (curry component-eq? current-as)
                         updated-sprite))
        e)))

(define (scale-a-sprite scale as)
  (lambda (g e)
    (define current-as (get-component e (curry component-eq? as)))
    (if current-as
        (let ([updated-sprite (~> current-as
                                  (scale-xy scale _)
                                  (multiply-x-offset scale _)
                                  (multiply-y-offset scale _))])
          (update-entity e (curry component-eq? current-as)
                         updated-sprite))
        e)))

; ==== MOTION-ITEM STRUCT ====
(struct motion-item (sprites delay motion-func start-func in-line?))

; ===== CUT-SCENE =====
; This is basically a mode-lambda/game-engine friendly version of (above ...)
; It can take any combination of images, text, sprites, or listof sprites

(define (page #:name         [name "Cut Scene"]
              #:bg           [bg #f]
              #:bg-color     [bg-color (color 50 50 50)]
              #:border-color [border-color 'white]
              #:duration     [duration #f]
              #:line-padding [line-padding 4]
              #:mode         [mode 'still]
              . items
              )
  (define bg-sprite (if bg
                        (ensure-sprite bg)
                        #f))
  
  ;item can be an image, text, sprite, or list of sprites
  (define (ensure-list-of-sprites item)
    (cond [(image-or-images? item)       (list (new-sprite item))]
          [(string-or-text-frame? item)  (list (if (eq? mode 'star-wars)
                                                   (text-sprite item #:font-size 14)
                                                   (new-sprite item #:color 'yellow)))]
          [(animated-sprite? item)       (list item)]
          [((listof sprite?) (flatten item)) (map ensure-sprite (flatten item))]
          ;[(motion-item? item)           (first item)]
          [(motion-item? item)             (motion-item-sprites item)]
          [else (error "That wasn't a valid cut-scene item!")]))
         
  (define (motion-item->motion-component item)
    (define delay (motion-item-delay item))
    (define motion-f (motion-item-motion-func item))
    (after-time delay (λ (g e)
                        (add-components e (every-tick motion-f)))))
  
  (define items-list
    (map ensure-list-of-sprites items))

  (define motion-components
    (map motion-item->motion-component (filter motion-item? items)))

  (define start-functions
    (map motion-item-start-func (filter motion-item? items)))

  (define outer-border-img (square 1 'solid 'black))
  (define inner-border-img (square 1 'solid border-color))
  (define box-img (square 1 'solid bg-color))

  ;(displayln "==== PRECOMPILING UI ====")
  (precompile! outer-border-img
               inner-border-img
               box-img)
  
  ;(displayln "==== PRECOMPILING PAGES ====")
  (apply precompile! (filter identity (flatten (append items-list
                                                       bg-sprite)))) ; Now works with sprites!
  

  (define (in-line-item? motion-item)
    (motion-item-in-line? motion-item))
  
  (define list-of-floating-motion-sprites
    (flatten (map ensure-list-of-sprites (filter (and/c motion-item?
                                                        (not/c in-line-item?))
                                                 items))))
  
  (define (get-sprite-height as)
    (if (member as list-of-floating-motion-sprites component-eq?) ;ignore non in-line motion items
        0
        (* (image-height (pick-frame as
                                 (animated-sprite-current-frame as)))
           (get-y-scale as))))

  (define (get-sprite-width as)
    (* (image-width (pick-frame as
                                 (animated-sprite-current-frame as)))
       (get-x-scale as)))
  
  (define (get-largest-sprite-height list-of-sprites)
    (define max-height (apply max (map get-sprite-height list-of-sprites)))
    (if (> max-height 0)
        (+ line-padding max-height)
        0))

  (define (get-largest-sprite-width list-of-sprites)
    (apply max (map get-sprite-width list-of-sprites)))
  
  (define total-height (apply + (map get-largest-sprite-height items-list)))
  (define total-width (get-largest-sprite-width (flatten items-list)))
  
  (define half-of-total (/ total-height 2))
  
  (define offset-items-list
    (flatten (for/list ([item items-list]
                        [i (range (length items-list))])
               (let ([last-height (apply + (map get-largest-sprite-height (take items-list i)))]
                     [half-height-item (/ (get-largest-sprite-height item) 2)])
                 (map (λ (s)
                        (if (member s list-of-floating-motion-sprites component-eq?)
                            s
                            (set-y-offset (+ last-height
                                         half-height-item
                                         (- half-of-total)
                                         (get-y-offset s)) s)))
                      item)
                 ))))

  (define (set-fullscreen-page)
    (lambda (g e)
      (define G-WIDTH (game-width g))
      (define G-HEIGHT (game-height g))
      (define OFFSET-WIDTH (+ (/ (- G-WIDTH total-width) 2) total-width))
      (define OFFSET-HEIGHT (+ (/ (- G-HEIGHT total-height) 2) total-height))
      (define modified-offset-items-list
        (cond [(eq? mode 'star-wars)    (map (compose (curry change-y-offset (- OFFSET-HEIGHT 50))
                                                      (curry set-as-layer "star-wars")) offset-items-list)]
              [(eq? mode 'star-wars-zoom-out)     (map (compose (curry scale-xy 4)
                                                      (curry multiply-x-offset 4)
                                                      (curry multiply-y-offset 4))     offset-items-list)]
              [(eq? mode 'scroll-up)    (map (curry change-y-offset OFFSET-HEIGHT)     offset-items-list)]
              [(eq? mode 'scroll-down)  (map (curry change-y-offset (- OFFSET-HEIGHT)) offset-items-list)]
              [(eq? mode 'scroll-right) (map (curry change-x-offset (- OFFSET-WIDTH))  offset-items-list)]
              [(eq? mode 'scroll-left)  (map (curry change-x-offset OFFSET-WIDTH)      offset-items-list)]
              [(eq? mode 'zoom-in)      (map (compose (curry scale-xy 0.25)
                                                      (curry multiply-x-offset 0.25)
                                                      (curry multiply-y-offset 0.25))  offset-items-list)]
              [(eq? mode 'zoom-out)     (map (compose (curry scale-xy 4)
                                                      (curry multiply-x-offset 4)
                                                      (curry multiply-y-offset 4))     offset-items-list)]
              [else offset-items-list]))
      (define bg-scale (if bg-sprite
                           (if (> (/ G-WIDTH G-HEIGHT) (/ (sprite-width bg-sprite) (sprite-height bg-sprite)))
                               (/ G-WIDTH  (sprite-width bg-sprite))
                               (/ G-HEIGHT (sprite-height bg-sprite)))
                           1.0))
      (define sprites-list
        (append modified-offset-items-list ;offset-items-list
                (if bg-sprite
                    (list (scale-xy bg-scale bg-sprite))
                    (bordered-box-sprite G-WIDTH G-HEIGHT #:color bg-color #:border-color border-color))))
      ;((change-sprite sprites-list) g e)))
      (~> e
          (remove-components _ animated-sprite?)
          (add-components _ (reverse sprites-list)
                            motion-components
                            ;on-start-components
                            )
          ((apply do-many start-functions) g _)
          )))

  (define dir (cond [(eq? mode 'star-wars)                                 270]
                    [(string-prefix? (symbol->string mode) "scroll-up")    270]
                    [(string-prefix? (symbol->string mode) "scroll-down")  90]
                    [(string-prefix? (symbol->string mode) "scroll-right") 0]
                    [(string-prefix? (symbol->string mode) "scroll-left")  180]
                    [else 0]))

  (define spd (if (eq? mode 'star-wars)
                  0.3
                  1))
  (define scroll-delay 1)
  
  (define (motion-fx g e)
    
    (cond [(or (string-prefix? (symbol->string mode) "scroll")
               (eq? mode 'star-wars))          ((apply do-many (map (curry move-a-sprite
                                                                           #:direction dir
                                                                           #:speed spd) offset-items-list)) g e)]
          [(eq? mode 'zoom-in)                 ((apply do-many (map (curry scale-a-sprite 1.01) offset-items-list)) g e)]
          [(or (eq? mode 'zoom-out)
               (eq? mode 'star-wars-zoom-out)) ((apply do-many (map (curry scale-a-sprite 0.99) offset-items-list)) g e)]
          [else e]))

  (sprite->entity empty-image
                  #:name       name
                  #:position   (posn 0 0)
                  #:components (layer "ui")
                               (hidden)
                               (storage "duration" duration)
                               (on-start (do-many (set-fullscreen-page)
                                                  (go-to-pos 'center) 
                                                  (do-after-time scroll-delay (λ (g e)
                                                                                (add-components e (every-tick motion-fx))))
                                                  show))
                               (if duration (after-time duration die) #f)
                               (on-key 'space die)
                               (on-key 'enter die)
                               ;(storable)   ;DON'T LEAVE THIS IN!
                               ))

; ===== END OF CUT-SCENE =====


; ===== MULTI-PAGE CUT-SCENE =====

(define (change-cutscene)
  (lambda (g e)
    ;change all sprites and change counter
    (define cut-scenes (get-storage-data "cut-scenes" e))
    (define current-scene-index (get-counter e))
    (define next-scene-index (min (add1 current-scene-index) (sub1 (length cut-scenes)))) ;capping at length - 1 just in case.
    (if (= current-scene-index (sub1 (length cut-scenes)))
        (die g e)
        (~> e
            (remove-components _ (or/c animated-sprite?
                                       after-time?
                                       every-tick?))
            (add-components _ (get-components (list-ref cut-scenes next-scene-index) (or/c animated-sprite?
                                                                                           every-tick?
                                                                                           on-start?
                                                                                           (and/c after-time?
                                                                                                  (λ (c)
                                                                                                    (not (eq? (after-time-func c) die))))))
                            (unless (not (get-storage-data "duration" (list-ref cut-scenes next-scene-index)))
                              (after-time (get-storage-data "duration" (list-ref cut-scenes next-scene-index)) (change-cutscene))))
            (update-entity _ counter? (counter next-scene-index))))))

(define (change-cutscene-by num)
  (lambda (g e)
    ;change all sprites and change counter
    (define cut-scenes (get-storage-data "cut-scenes" e))
    (define current-scene-index (get-counter e))
    (define next-scene-index (max 0 (min (+ num current-scene-index) (sub1 (length cut-scenes))))) ;capping at length - 1 just in case.
    (if (= current-scene-index (sub1 (length cut-scenes)))
        (die g e)
        (~> e
            (remove-components _ (or/c animated-sprite?
                                       after-time?
                                       every-tick?))
            (add-components _ (get-components (list-ref cut-scenes next-scene-index) (or/c animated-sprite?
                                                                                           every-tick?
                                                                                           on-start?
                                                                                           (and/c after-time?
                                                                                                  (λ (c)
                                                                                                    (not (eq? (after-time-func c) die))))))
                            (unless (not (get-storage-data "duration" (list-ref cut-scenes next-scene-index)))
                              (after-time (get-storage-data "duration" (list-ref cut-scenes next-scene-index)) (change-cutscene))))
            (update-entity _ counter? (counter next-scene-index))))))

(define (cutscene . pages)
  ; for now, pages is a list of page entities
  ; maybe turn it into a scene/page struct?
  ;(apply precompile! pages) ; precompile! takes images or entities but NOT sprites?
  (define pages-or-default
    (if (empty? pages)
        (list (page "Once upon a time..."))
        pages))
  (define (set-first-page)
    (lambda (g e)
      (define cut-scenes (get-storage-data "cut-scenes" e))
      (~> e
          (remove-components _ animated-sprite?)
          (add-components _ (get-components (first cut-scenes) (or/c animated-sprite?
                                                                     every-tick?
                                                                     (and/c after-time?
                                                                            (λ (c)
                                                                              (not (eq? (after-time-func c) die))))))
                            (unless (not (get-storage-data "duration" (first cut-scenes)))
                              (after-time (get-storage-data "duration" (first cut-scenes)) (change-cutscene))))
          )))

  (define (bake-and-store-pages)
    (lambda (g e)
      (define (bake-page page)
        (~> page
            ((apply do-many (map on-start-func (get-components page on-start?))) g _)
            (remove-components _ on-start?)))
      (define baked-pages (map bake-page pages-or-default))
      (add-components e (storage "cut-scenes" baked-pages))))
  
  (sprite->entity empty-image ;(reverse (get-components (first pages) animated-sprite?))
                  #:name "Multi Cut Scene"
                  #:position (posn 0 0)
                  #:components (layer "ui")
                               (hidden)
                               ;(apply precompiler (flatten (map (curryr get-components image-animated-sprite?) pages)))
                               ;;(storage "cut-scenes" pages)
                               (counter 0)
                               ;(storable)
                               (on-start (do-many (bake-and-store-pages)
                                                  (set-first-page)
                                                  (go-to-pos 'center)
                                                  show))
                               (on-key 'enter (change-cutscene))))

; ===== END OF MULTI-PAGE CUT-SCENE =====

; ==== SPRITE FUNCTIONS ====
(define (get-largest-sprite-width list-of-sprites)
    (apply max (map get-sprite-width list-of-sprites)))

(define (get-largest-sprite-height list-of-sprites)
    (apply max (map get-sprite-height list-of-sprites)))

; ===== MOTION FUNCTIONS FOR INDIVIDUAL LINES =====
(define (scroll-right item #:speed [spd 1] #:delay [delay 2] #:in-line? [in-line? #f])
  (define (ensure-list-of-sprites item)
    (cond [(image-or-images? item)       (list (new-sprite item))]
          [(string-or-text-frame? item)  (list (new-sprite item #:color 'yellow))]
          [(animated-sprite? item)       (list item)]
          [((listof sprite?) (flatten item)) (map ensure-sprite (flatten item))]
          [else (error "That wasn't a valid cut-scene item!")]))
  (define sprite-list (ensure-list-of-sprites item))
  (define item-width (get-largest-sprite-width sprite-list))
  (define (in-sprite-list? as)
    (member as sprite-list component-eq?))
  (define (start-line-offscreen g e)
    (define G-WIDTH (game-width g))
    (define current-sprite-list (get-components e in-sprite-list?))
    (define OFFSET-WIDTH (+ (/ (- G-WIDTH item-width) 2) item-width))
    (define adjusted-sprites (map (curry change-x-offset (- OFFSET-WIDTH)) current-sprite-list))
    (~> e
        (remove-components _ in-sprite-list?)
        (add-components _ adjusted-sprites)))
  (define (scroll-fx g e)
    ((apply do-many (map (curry move-a-sprite
                                #:direction 0
                                #:speed spd) sprite-list)) g e))
  (motion-item sprite-list
               delay
               scroll-fx
               start-line-offscreen
               in-line?)
  )

(define (scroll-left item #:speed [spd 1] #:delay [delay 2] #:in-line? [in-line? #f])
  (define (ensure-list-of-sprites item)
    (cond [(image-or-images? item)       (list (new-sprite item))]
          [(string-or-text-frame? item)  (list (new-sprite item #:color 'yellow))]
          [(animated-sprite? item)       (list item)]
          [((listof sprite?) (flatten item)) (map ensure-sprite (flatten item))]
          [else (error "That wasn't a valid cut-scene item!")]))
  (define sprite-list (ensure-list-of-sprites item))
  (define item-width (get-largest-sprite-width sprite-list))
  (define (in-sprite-list? as)
    (member as sprite-list component-eq?))
  (define (start-line-offscreen g e)
    (define G-WIDTH (game-width g))
    (define current-sprite-list (get-components e in-sprite-list?))
    (define OFFSET-WIDTH (+ (/ (- G-WIDTH item-width) 2) item-width))
    (define adjusted-sprites (map (curry change-x-offset (+ OFFSET-WIDTH)) current-sprite-list))
    (~> e
        (remove-components _ in-sprite-list?)
        (add-components _ adjusted-sprites)))
  (define (scroll-fx g e)
    ((apply do-many (map (curry move-a-sprite
                                #:direction 180
                                #:speed spd) sprite-list)) g e))
  (motion-item sprite-list
               delay
               scroll-fx
               start-line-offscreen
               in-line?))

(define (scroll-up item #:speed [spd 1] #:delay [delay 2] #:in-line? [in-line? #f])
  (define (ensure-list-of-sprites item)
    (cond [(image-or-images? item)       (list (new-sprite item))]
          [(string-or-text-frame? item)  (list (new-sprite item #:color 'yellow))]
          [(animated-sprite? item)       (list item)]
          [((listof sprite?) (flatten item)) (map ensure-sprite (flatten item))]
          [else (error "That wasn't a valid cut-scene item!")]))
  (define sprite-list (ensure-list-of-sprites item))
  (define item-height (get-largest-sprite-height sprite-list))
  (define (in-sprite-list? as)
    (member as sprite-list component-eq?))
  (define (start-line-offscreen g e)
    (define G-HEIGHT (game-height g))
    ;(define current-sprite-list (get-components e in-sprite-list?))
    (define OFFSET-HEIGHT (+ (/ (- G-HEIGHT item-height) 2) item-height))
    (define adjusted-sprites (map (curry change-y-offset OFFSET-HEIGHT) sprite-list))
    (~> e
        (remove-components _ in-sprite-list?)
        (add-components _ adjusted-sprites)))
  (define (scroll-fx g e)
    ((apply do-many (map (curry move-a-sprite
                                #:direction 270
                                #:speed spd) sprite-list)) g e))
  (motion-item sprite-list
               delay
               scroll-fx
               start-line-offscreen
               in-line?))

(define (scroll-down item #:speed [spd 1] #:delay [delay 2] #:in-line? [in-line? #f])
  (define (ensure-list-of-sprites item)
    (cond [(image-or-images? item)       (list (new-sprite item))]
          [(string-or-text-frame? item)  (list (new-sprite item #:color 'yellow))]
          [(animated-sprite? item)       (list item)]
          [((listof sprite?) (flatten item)) (map ensure-sprite (flatten item))]
          [else (error "That wasn't a valid cut-scene item!")]))
  (define sprite-list (ensure-list-of-sprites item))
  (define item-height (get-largest-sprite-height sprite-list))
  (define (in-sprite-list? as)
    (member as sprite-list component-eq?))
  (define (start-line-offscreen g e)
    (define G-HEIGHT (game-height g))
    ;(define current-sprite-list (get-components e in-sprite-list?))
    (define OFFSET-HEIGHT (+ (/ (- G-HEIGHT item-height) 2) item-height))
    (define adjusted-sprites (map (curry change-y-offset (- OFFSET-HEIGHT)) sprite-list))
    (~> e
        (remove-components _ in-sprite-list?)
        (add-components _ adjusted-sprites)))
  (define (scroll-fx g e)
    ((apply do-many (map (curry move-a-sprite
                                #:direction 90
                                #:speed spd) sprite-list)) g e))
  (motion-item sprite-list
               delay
               scroll-fx
               start-line-offscreen
               in-line?))

; ==== SCROLL LINE ITEM FROM CENTER ====
(define (scroll-right-from-center item #:speed [spd 1] #:delay [delay 2] #:in-line? [in-line? #f])
  (define (ensure-list-of-sprites item)
    (cond [(image-or-images? item)       (list (new-sprite item))]
          [(string-or-text-frame? item)  (list (new-sprite item #:color 'yellow))]
          [(animated-sprite? item)       (list item)]
          [((listof sprite?) (flatten item)) (map ensure-sprite (flatten item))]
          [else (error "That wasn't a valid cut-scene item!")]))
  (define sprite-list (ensure-list-of-sprites item))
  (define (scroll-fx g e)
    ((apply do-many (map (curry move-a-sprite
                                #:direction 0
                                #:speed spd) sprite-list)) g e)) 
  (motion-item sprite-list
               delay
               scroll-fx
               (λ (g e) e)
               in-line?))

(define (scroll-left-from-center item #:speed [spd 1] #:delay [delay 2] #:in-line? [in-line? #f])
  (define (ensure-list-of-sprites item)
    (cond [(image-or-images? item)       (list (new-sprite item))]
          [(string-or-text-frame? item)  (list (new-sprite item #:color 'yellow))]
          [(animated-sprite? item)       (list item)]
          [((listof sprite?) (flatten item)) (map ensure-sprite (flatten item))]
          [else (error "That wasn't a valid cut-scene item!")]))
  (define sprite-list (ensure-list-of-sprites item))
  (define (scroll-fx g e)
    ((apply do-many (map (curry move-a-sprite
                                #:direction 180
                                #:speed spd) sprite-list)) g e))
  (motion-item sprite-list
               delay
               scroll-fx
               (λ (g e) e)
               in-line?))

(define (scroll-up-from-center item #:speed [spd 1] #:delay [delay 2] #:in-line? [in-line? #f])
  (define (ensure-list-of-sprites item)
    (cond [(image-or-images? item)       (list (new-sprite item))]
          [(string-or-text-frame? item)  (list (new-sprite item #:color 'yellow))]
          [(animated-sprite? item)       (list item)]
          [((listof sprite?) (flatten item)) (map ensure-sprite (flatten item))]
          [else (error "That wasn't a valid cut-scene item!")]))
  (define sprite-list (ensure-list-of-sprites item))
  (define (scroll-fx g e)
    ((apply do-many (map (curry move-a-sprite
                                #:direction 270
                                #:speed spd) sprite-list)) g e))
  (motion-item sprite-list
               delay
               scroll-fx
               (λ (g e) e)
               in-line?))

(define (scroll-down-from-center item #:speed [spd 1] #:delay [delay 2] #:in-line? [in-line? #f])
  (define (ensure-list-of-sprites item)
    (cond [(image-or-images? item)       (list (new-sprite item))]
          [(string-or-text-frame? item)  (list (new-sprite item #:color 'yellow))]
          [(animated-sprite? item)       (list item)]
          [((listof sprite?) (flatten item)) (map ensure-sprite (flatten item))]
          [else (error "That wasn't a valid cut-scene item!")]))
  (define sprite-list (ensure-list-of-sprites item))
  (define (scroll-fx g e)
    ((apply do-many (map (curry move-a-sprite
                                #:direction 90
                                #:speed spd) sprite-list)) g e))
  (motion-item sprite-list
               delay
               scroll-fx
               (λ (g e) e)
               in-line?))

; ==== FADE FUNCTIONS (SPAWNS AN OVERLAY) ====
(define (fade-in #:step [step 2] #:color [color 'black])
  (define c (name->color color))
  (define r-val (color-red c))
  (define g-val (color-green c))
  (define b-val (color-blue c))
  (apply precompile! (map (λ (a)
                            (square 1 'solid (make-color r-val g-val b-val a))) (range 256)))
  (lambda (g e)
    (define alpha-val 255)
    (define GW (game-width g))
    (define GH (game-height g))
    (define overlay-sprite (new-sprite (square 1 'solid (make-color r-val g-val b-val alpha-val))
                                       #:x-scale GW
                                       #:y-scale GH))
    (define (update-overlay-sprite g e)
      (define new-alpha (max 0 (- (get-storage-data "alpha" e) step)))
      (define new-overlay-sprite
        (new-sprite (square 1 'solid (make-color r-val g-val b-val new-alpha))
                    #:x-scale GW
                    #:y-scale GH))
      (~> e
          (set-storage "alpha" _ new-alpha)
          (update-entity _ animated-sprite? new-overlay-sprite)))
    (define overlay-entity
      (sprite->entity overlay-sprite
                      #:name "fade-in"
                      #:position (posn (/ GW 2) (/ GH 2))
                      #:components (layer "ui")
                                   (storage "alpha" alpha-val)
                                   (every-tick update-overlay-sprite)
                                   (after-time (exact-round (/ 255 step)) die)))
    ((spawn overlay-entity #:relative? #f) g e)))

(define (fade-out #:step [step 2] #:color [color 'black])
  (define c (name->color color))
  (define r-val (color-red c))
  (define g-val (color-green c))
  (define b-val (color-blue c))
  (apply precompile! (map (λ (a)
                            (square 1 'solid (make-color r-val g-val b-val a))) (range 256)))
  (lambda (g e)
    (define alpha-val 0)
    (define GW (game-width g))
    (define GH (game-height g))
    (define overlay-sprite (new-sprite (square 1 'solid (make-color r-val g-val b-val alpha-val))
                                       #:x-scale GW
                                       #:y-scale GH))
    (define (update-overlay-sprite g e)
      (define new-alpha (min 255 (+ (get-storage-data "alpha" e) step)))
      (define new-overlay-sprite
        (new-sprite (square 1 'solid (make-color r-val g-val b-val new-alpha))
                    #:x-scale GW
                    #:y-scale GH))
      (~> e
          (set-storage "alpha" _ new-alpha)
          (update-entity _ animated-sprite? new-overlay-sprite)))
    (define overlay-entity
      (sprite->entity overlay-sprite
                      #:name "fade-in"
                      #:position (posn (/ GW 2) (/ GH 2))
                      #:components (layer "ui")
                                   (storage "alpha" alpha-val)
                                   (every-tick update-overlay-sprite)
                                   (after-time (exact-round (/ 255 step)) die)))
    ((spawn overlay-entity #:relative? #f) g e)))
                            
; ================================================================