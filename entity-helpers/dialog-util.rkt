#lang racket

(provide animated-dialog
         fast-animated-dialog
         pad
         draw-title
         draw-dialog
         draw-dialog-background
         draw-dialog-text
         draw-dialog-sheet
         draw-dialog-sheet-text
         
         ;draw-dialog-list
         fast-dialog-list
         
         ;draw-crafting-list
         fast-crafting-list
         
         ;draw-dialog-lg
         fast-dialog-lg
         
         draw-avatar-box
         ;TODO: make fast-avatar-box
         
         dialog->sprites
         dialog->response-sprites
         
         next-dialog
         next-response
         
         reached-frame?
         create-dialog
         dialog-lg
         dialog-list
         
         start-blips
         stop-blips
         get-dialog-selection
         all-dialog-closed?
         npc-spoke-and-near?
         player-spoke-and-near?
         ready-to-speak-and-near?
         npc-dialog-open?
         player-dialog-open?
         last-dialog?
         not-last-dialog?
         (rename-out (last-dialog? npc-last-dialog?))
         )

(require "../game-entities.rkt")
(require "../components/animated-sprite.rkt")
(require "../components/counter.rkt")
;(require "../components/spawn-dialog.rkt")
(require "../components/spawn-once.rkt")
(require "../components/do-every.rkt")
(require "../components/after-time.rkt")
(require "../components/lock-to.rkt")
(require "../components/on-key.rkt")
(require "../components/on-start.rkt")
(require "../components/on-rule.rkt")
(require "../components/sound-stream.rkt")
(require "../components/dialog.rkt")
(require "../components/storage.rkt")
(require "../component-util.rkt")
(require "../entity-helpers/movement-util.rkt")
(require "../entity-helpers/sprite-util.rkt")
(require "../entity-helpers/rgb-hsb.rkt")
(require "../entity-helpers/ui-util.rkt")

(require 2htdp/image
         posn
         threading)


(define (pad image w-pad h-pad)
  (define w (image-width image))
  (define h (image-height image))
  (define w-padding (* w-pad 2))
  (define h-padding (* h-pad 2))
  (freeze (overlay (freeze image)
           (rectangle (+ w w-padding) (+ h h-padding) "solid" "transparent")))
  #;(overlay image
           (rectangle (+ w w-padding) (+ h h-padding) "solid" "transparent"))
  )

(define (animated-dialog msg game-width #:skip [skip 1])
  (sheet->sprite (draw-dialog-sheet-text msg game-width #:skip skip)
                 #:rows       1
                 #:columns    (+ 2 (exact-floor (/ (string-length msg) skip)))
                 #:row-number 1
                 #:speed      1))

(define (draw-title msg #:font-size (font-size 24))
  (define message (text msg font-size "yellow"))
  (overlay message
           (rectangle (+ 12 (image-width message)) (+ 12 (image-height message)) "outline" (pen "white" 2 "solid" "butt" "bevel"))
           (rectangle (+ 16 (image-width message)) (+ 16 (image-height message)) "solid" (make-color 20 20 20 150))))

(define (draw-dialog msg)
  (define message (text msg 12 "yellow"))
  (overlay message
           (draw-dialog-background msg)))

(define (draw-dialog-background msg)
  
  (define message (text msg 12 "yellow"))
  
  (overlay
   (rectangle (+ 12 (image-width message)) (+ 8  (image-height message)) "outline" (pen "white" 2 "solid" "butt" "bevel"))
   (rectangle (+ 16 (image-width message)) (+ 12 (image-height message)) "solid"  (make-color 20 20 20 150))))

(define (draw-dialog-text msg game-width)
  (define message (text msg 18 "yellow"))
  (overlay/align "left" "middle"
                 (pad message 6 8)
                 (rectangle (/ (* game-width 3) 4) (+ 12 (image-height message)) "solid" "transparent")))

(define (draw-dialog-sheet msg #:skip [skip 1])
  (define msg-list (string->list msg))
  (define msg-len (length msg-list))
  (apply beside
         (for/list ([i (append (range 0 (add1 msg-len) (min skip msg-len)) (list msg-len))])
           (define message (text msg 18 "yellow"))
           (overlay/align "left" "middle"
                          (pad (text (list->string (take msg-list i)) 18 "yellow") 6 8)
                          (overlay
                           (rectangle (+ 12 (image-width message)) (+ 12 (image-height message)) "outline" (pen "white" 2 "solid" "butt" "bevel"))
                           (rectangle (+ 16 (image-width message)) (+ 16 (image-height message)) "solid"  (make-color 20 20 20 150)))))))

(define (draw-dialog-sheet-text msg game-width #:skip [skip 1])
  (define msg-list (string->list msg))
  (define msg-len (length msg-list))
  (apply beside
         (for/list ([i (append (range 0 (add1 msg-len) (min skip msg-len)) (list msg-len))])
           (define message (text msg 18 "yellow"))
           (overlay/align "left" "middle"
                          (pad (text (list->string (take msg-list i)) 18 "yellow") 6 8)
                          (rectangle (/ (* game-width 3) 4) (+ 12 (image-height message)) "solid" "transparent")))))

(define (draw-crafting-list msg-list icon-list font-size selection)
  (define list-of-entries (map (λ (msg icon) (freeze (beside
                                              (pad (scale-to-fit icon (image-height (text "" font-size "yellow"))) 4 2)
                                              (pad (text msg font-size "yellow") 4 2)))) msg-list icon-list))
    (define message-list (if (= 1 (length list-of-entries))
                             (first list-of-entries)
                             (apply (curry above/align "left") list-of-entries)))
  (overlay message-list
           (rectangle (+ 12 (image-width message-list)) (+ 12 (image-height message-list)) "outline" (pen "white" 2 "solid" "butt" "bevel"))
           (rectangle (+ 16 (image-width message-list)) (+ 16 (image-height message-list)) "solid"  (make-color 20 20 20 150))))

(define (fast-crafting-list msg-list icon-list [selected-item-index 0])
  (define GAME-MAX-WIDTH (- (/ 480 10) 2))
  (define MSG-MAX-WIDTH (apply max (map string-length msg-list)))
  (define MSG-WIDTH (min MSG-MAX-WIDTH GAME-MAX-WIDTH))
  (define LINE-HEIGHT 24)
  (define (pad-text t)
    (~a t
        #:width MSG-WIDTH
        #:limit-marker "..."
        #:align 'left))
  (define message-list (map pad-text msg-list))
  (define num-items (length msg-list))
  (define main-box-width  (* (+ MSG-WIDTH 4) 10))
  (define main-box-height (* LINE-HEIGHT num-items))

  ; ==== ADDING ICONS TO EACH MENU ITEM ====
  ;for now, icon-list is a list of images
  (define (option-with-icon msg icon)
    (list (new-sprite (scale-to-fit icon LINE-HEIGHT)
                      #:x-offset (- (/ main-box-width 2)))
          (new-sprite msg
                      #:animate #f
                      #:color 'yellow
                      #:x-offset (+ (/ LINE-HEIGHT 2)))))

  (define list-of-entries (map option-with-icon message-list icon-list))

  ; ========================================
  
  (define offset-sprite-list
    (flatten (for/list ([ls list-of-entries]
                        [i (range (length message-list))])
               (map (λ (s)
                      (~> s
                          (set-y-offset (+ (/ LINE-HEIGHT 2)
                                           (- (* i LINE-HEIGHT)
                                              (/ main-box-height 2))
                                           (get-y-offset s)) _)))
                    ls))))
  
  (define selection-box-offset (+ (/ LINE-HEIGHT 2)
                                  (- (* selected-item-index LINE-HEIGHT)
                                     (/ main-box-height 2))))
  (define selection-image (square 1 'solid (color 0 255 255 100)))
  (precompile! selection-image)
  (define selection-box-sprite (new-sprite selection-image
                                           #:x-scale (+ main-box-width LINE-HEIGHT)
                                           #:y-scale LINE-HEIGHT
                                           #:y-offset selection-box-offset))
  (append offset-sprite-list
          (list selection-box-sprite)
          (bordered-box-sprite (+ (+ main-box-width LINE-HEIGHT) 10)
                               (+ main-box-height 10))))

(define (draw-dialog-list msg-list font-size selection)
  (define message-list (foldr (lambda (new-text text-img)
                                (above/align "left"
                                             (pad (text new-text font-size "yellow") 4 4)
                                             text-img))
                              empty-image
                              msg-list))
  (overlay message-list
           (rectangle (+ 12 (image-width message-list)) (+ 12 (image-height message-list)) "outline" (pen "white" 2 "solid" "butt" "bevel"))
           (rectangle (+ 16 (image-width message-list)) (+ 16 (image-height message-list)) "solid"  (make-color 20 20 20 150))))

(define (fast-dialog-list msg-list [selected-item-index 0])
  (define GAME-MAX-WIDTH (- (/ 480 10) 2))
  (define MSG-MAX-WIDTH (apply max (map string-length msg-list)))
  (define MSG-WIDTH (min MSG-MAX-WIDTH GAME-MAX-WIDTH))
  (define LINE-HEIGHT 24)
  (define (pad-text t)
    (~a t
        #:width MSG-WIDTH
        #:limit-marker "..."
        #:align 'left))
  (define message-list (map pad-text msg-list))
  (define num-items (length msg-list))
  (define main-box-width  (* (+ MSG-WIDTH 2) 10))
  (define main-box-height (* LINE-HEIGHT num-items))
  (define offset-sprite-list
    (for/list ([msg message-list]
               [i (range (length message-list))])
      (new-sprite msg
                  #:animate #f
                  #:color 'yellow
                  #:y-offset (+ (/ LINE-HEIGHT 2)
                                (- (* i LINE-HEIGHT)
                                   (/ main-box-height 2))))))
  (define selection-box-offset (+ (/ LINE-HEIGHT 2)
                                  (- (* selected-item-index LINE-HEIGHT)
                                     (/ main-box-height 2))))
  (define selection-image (square 1 'solid (color 0 255 255 100)))
  (precompile! selection-image)
  (define selection-box-sprite (new-sprite selection-image
                                           #:x-scale main-box-width
                                           #:y-scale LINE-HEIGHT
                                           #:y-offset selection-box-offset))
  (append offset-sprite-list
          (list selection-box-sprite)
          (bordered-box-sprite (+ main-box-width 10)
                               (+ main-box-height 10))))

(define (draw-avatar-box e)
  (define avatar-img (pick-frame-original (get-component e animated-sprite?) 0))
  (freeze (overlay (rectangle 56 56 "outline" (pen "white" 2 "solid" "butt" "bevel"))
                       (rectangle 58 58 "outline" (pen "black" 2 "solid" "butt" "bevel"))
                       (place-image
                        (freeze (scale 2 avatar-img))
                        30 30
                        (rectangle 60 60 "solid" (make-color 255 255 255 100))))))

(define (next-dialog dialog-list #:sound [rsound #f])
  (lambda (g e)
    (define WIDTH (game-width g))
    (define HEIGHT (game-height g))
    (define TEXT-WIDTH (* WIDTH .8))
    (define dialog-index (get-counter e))
    ;(displayln (~a "CURRENT DIALOG: " dialog-index))
    (define dialog-length (length dialog-list))
    (define name (get-name e))
    (define avatar-box (draw-avatar-box e))
    (define message-entity (create-dialog dialog-list
                                          name
                                          (posn (+ (- WIDTH TEXT-WIDTH) (/ TEXT-WIDTH 2) -6) ;(/ (* WIDTH 2.3) 4)
                                                (- HEIGHT 40))
                                          #:sound rsound))
    (update-entity (add-component e
                                  (spawn-once #:relative? #f (dialog-lg avatar-box name message-entity WIDTH #:delay 5)))
                   counter?
                   (counter (add1 dialog-index)))))

(define (next-response response-list #:sound [rsound #f])
  (lambda (g e)
    (define WIDTH (game-width g))
    (define HEIGHT (game-height g))
    (define TEXT-WIDTH (* WIDTH .8))
    (define player-dialog-index (get-counter (get-entity "player" g)))
    (define npc-dialog-index (get-counter e))
    (define response-length (length (list-ref response-list player-dialog-index)))
    (define name (get-name e))
    (define avatar-box (draw-avatar-box e))
    (define message-entity (create-dialog response-list name (posn (+ (- WIDTH TEXT-WIDTH) (/ TEXT-WIDTH 2) -6) ;(/ (* WIDTH 2.5) 4)
                                                                   (- HEIGHT 40))
                                          #:sound rsound))
    (add-component (update-entity e counter? (counter (add1 npc-dialog-index)))
                   (spawn-once #:relative? #f  (dialog-lg avatar-box name message-entity WIDTH #:delay 10)))))

(define (reached-frame? g e)
  (define as (get-component e animated-sprite?))
  (define total-frames (animated-sprite-total-frames as))
  (define current-frame (animated-sprite-current-frame as))
  (define end-frame (sub1 (animated-sprite-total-frames (get-component e animated-sprite?))))
  (if (> total-frames 1)
      (= current-frame end-frame)
      #f))


(define (change-dialog-sprite)
  (lambda (g e)
    (define npc-dialog-index (get-dialog-index e))
    (define player-dialog-index (get-counter (get-entity "player" g)))
    (define npc-dialog-sprites (get-dialog-sprites e))
    (define simple-dialog? (animated-sprite? (first npc-dialog-sprites)))
    (define dialog-sprite (if simple-dialog?
                              (list-ref npc-dialog-sprites npc-dialog-index)
                              (list-ref (list-ref npc-dialog-sprites player-dialog-index) npc-dialog-index)))
    ((do-many (change-sprite dialog-sprite)
              (set-dialog-index (add1 npc-dialog-index))) g e)))

(define (stop-dialog-scroll)
  (lambda (g e)
    (define as (get-component e animated-sprite?))
    ;((change-sprite (new-sprite (pick-frame dialog-sprite (sub1 (animated-sprite-total-frames dialog-sprite))) 1 #:color 'yellow)) g e)
    (update-entity e
                   animated-sprite?
                   (struct-copy animated-sprite as
                                [animate? #f]))
    ))
    
(define (create-dialog dialog-list name pos #:delay [delay-time 0] #:sound [rsound #f])
  (sprite->entity empty-image ;(draw-dialog msg)
                  #:name       "npc dialog"
                  #:position   pos ;(posn 0 -40)
                  #:components (static)
                               (hidden)
                               (layer "ui")
                               (dialog dialog-list 0)
                               ;(on-key 'space die)
                               (on-key 'enter #:rule last-dialog? die)
                               (on-key 'enter #:rule not-last-dialog? (if rsound
                                                                          (do-many (change-dialog-sprite)
                                                                                   (stop-blips)
                                                                                   (start-blips name rsound))
                                                                          (change-dialog-sprite)))
                               ;(on-start (go-to (/ (* WIDTH 2.5) 4) (- HEIGHT 40)))
                               (on-rule reached-frame? #;(reached-frame? (sub1 (animated-sprite-total-frames dialog-sprite))) (do-many (stop-dialog-scroll);(change-sprite (new-sprite (pick-frame dialog-sprite (sub1 (animated-sprite-total-frames dialog-sprite))) 1))
                                                                                                                                       (stop-blips)))
                               (after-time delay-time (if rsound
                                                          (do-many (change-dialog-sprite) ;(change-sprite dialog-sprite)
                                                                   show
                                                                   (start-blips name rsound))
                                                          (do-many (change-dialog-sprite) ;(change-sprite dialog-sprite)
                                                                   show)))
                  ))

(define (draw-dialog-lg name avatar game-width)
  (define padded-name (~a name
                          #:min-width 18
                          #:max-width 80
                          #:limit-marker "..."
                          #:align 'center))
  (above/align "left"
               (draw-dialog padded-name)
               (place-image/align (pad avatar 16 16)
                                  0 40 "left" "center"
                                  (overlay (rectangle (- game-width 4) 76 "outline" (pen "white" 2 "solid" "butt" "bevel"))
                                           (rectangle game-width       80 "solid"  (make-color 20 20 20 150))))))

(define (fast-dialog-lg name avatar game-width)
  (define NAME-MAX-WIDTH (- (/ game-width 10) 2))
  (define padded-name (~a name
                          #:min-width (if (even? (string-length name)) 10 11)
                          #:max-width NAME-MAX-WIDTH
                          #:limit-marker "..."
                          #:align 'center))
  (define name-box-width (* (+ 2 (string-length padded-name)) 10))
  (define name-box-height 30)
  (define outer-border-image (square 1 'solid 'black))
  (define border-image (square 1 'solid 'white))
  (define box-image (square 1 'solid 'dimgray))

  (define main-bordered-box (bordered-box-sprite game-width 80))

  (define name-bordered-box (map (compose (curry set-x-offset (- (/ name-box-width 2) (/ game-width 2)))
                                          (curry set-y-offset (- (+ 40 (/ name-box-height 2)))))
                                 (bordered-box-sprite name-box-width name-box-height)))
  
  (define name-text-sprite (new-sprite padded-name
                                       #:color 'yellow
                                       #:x-offset (- (/ name-box-width 2) (/ game-width 2))
                                       #:y-offset (- (+ 40 (/ name-box-height 2)))))
  (define avatar-sprite (new-sprite avatar
                                    #:animate #f
                                    #:x-offset (- (* game-width 0.1) ;(+ 16 (/ (image-width avatar) 2))
                                                  (/ game-width 2))))
  (flatten (list name-text-sprite
                 name-bordered-box
                 avatar-sprite
                 main-bordered-box)))
     
(define (dialog-lg avatar name message-entity game-width #:delay [delay-time 0])
  (sprite->entity (fast-dialog-lg name avatar game-width)
                  #:name       "dialog bg"
                  #:position   (posn 0 0)
                  #:components (static)
                               (hidden)
                               (layer "ui")
                               (on-key 'enter #:rule last-dialog? die)
                               (on-start (go-to-pos-inside 'bottom-center))
                               (after-time delay-time (do-many show
                                                               (spawn #:relative? #f message-entity)))
                  ))


(define (dialog-list dialog-list pos
                     #:selection    [selection 0]
                     #:select-sound [select-sound #f])
  (define LINE-HEIGHT 24)
  (define main-box-height (* LINE-HEIGHT (length dialog-list)))
  (define dialog-list-sprites (fast-dialog-list dialog-list selection))

  (define (next-option)
    (lambda (g e)
      (define new-index (modulo (add1 (get-counter e)) (length dialog-list)))
      (define selection-box-sprite
        (get-component e (curry component-eq? (get-storage-data "selection-box-sprite" e))))
      
      (define new-offset (+ (/ LINE-HEIGHT 2)
                            (- (* new-index LINE-HEIGHT)
                               (/ main-box-height 2))))
      
      (define new-selection-box-sprite
        (set-y-offset new-offset selection-box-sprite))
      (~> e
          (update-entity _ (curry component-eq? selection-box-sprite) new-selection-box-sprite)
          (update-entity _ counter? (counter new-index)))))

  (define (previous-option)
    (lambda (g e)
      (define new-index (modulo (sub1 (get-counter e)) (length dialog-list)))
      (define selection-box-sprite
        (get-component e (curry component-eq? (get-storage-data "selection-box-sprite" e))))
      
      (define new-offset (+ (/ LINE-HEIGHT 2)
                            (- (* new-index LINE-HEIGHT)
                               (/ main-box-height 2))))
      
      (define new-selection-box-sprite
        (set-y-offset new-offset selection-box-sprite))
      (~> e
          (update-entity _ (curry component-eq? selection-box-sprite) new-selection-box-sprite)
          (update-entity _ counter? (counter new-index)))))
  
  (sprite->entity dialog-list-sprites
                  #:name       "player dialog"
                  #:position   pos
                  #:components (static)
                               (hidden)
                               (layer "ui")
                               (counter selection)
                               (storage "selection-box-sprite" (list-ref dialog-list-sprites (length dialog-list)))
                               (on-start (do-many (go-to-pos 'center)
                                                  show
                                                  #|(spawn (dialog-selection dialog-list
                                                                           selection-width
                                                                           font-size
                                                                           selection
                                                                           rsound)
                                                         #:relative? #f)|#
                                                  ))
                               (on-key 'enter die)
                               (on-key 'up   (if select-sound
                                                 (do-many (previous-option)
                                                          (play-sound-from "player" select-sound))
                                                 (previous-option)))
                               (on-key 'down (if select-sound
                                                 (do-many (next-option)
                                                          (play-sound-from "player" select-sound))
                                                 (next-option)))
                               ))

; === DIALOG BLIPS ===
(define (start-blips name rsound)
  ;(displayln (~a "PLAYING SOUND FROM: " name))
  (lambda (g e)
    (define as (get-component e animated-sprite?))
    (define total-frames (animated-sprite-total-frames as))
    (if (> total-frames 1)
        (add-component e (do-every 2 (play-sound-from name rsound)))
        ((play-sound-from name rsound) g e))))

(define (stop-blips)
  (lambda (g e)
    (remove-component e do-every?)))

(define (get-dialog-selection)
  (lambda (g e)
    (define selection (get-counter (get-entity "player dialog" g)))
    ;(displayln (~a "Player Selection: " selection))
    (update-entity e counter? (counter selection))))

(define (animated-dialog-string msg #:skip [skip 1])
  (define msg-list (string->list msg))
  (define msg-len  (length msg-list))
  (for/list ([i (append (range 0 (add1 msg-len) (min skip msg-len)) (list msg-len))])
    (list->string (take msg-list i))))

(define (format-dialog msg game-width)
  (~a msg
      #:width (- (exact-round (* game-width 0.8 0.1)) 2)
      ;#:limit-marker "..."
      ))

(define (fast-animated-dialog msg game-width #:skip [skip 1])
  (new-sprite (map (curryr format-dialog game-width)
                   (animated-dialog-string msg #:skip skip))
              #:color 'yellow))

; === GENERIC SPRITE GENERATORS ===
(define (dialog->response-sprites dialog-list #:game-width game-width #:animated [animated? #t] #:speed [spd 2])
  (map (lambda (response-list)
         (map (lambda (msg)
                (if animated?
                    ;(animated-dialog msg game-width #:skip spd)
                    ;(new-sprite (draw-dialog-text msg game-width))
                    (fast-animated-dialog msg game-width #:skip spd)
                    (new-sprite (format-dialog msg game-width) #:color 'yellow)
                    )
                ) response-list)) dialog-list))

(define (dialog->sprites dialog-list #:game-width game-width #:animated [animated? #t] #:speed [spd 2])
  (map (lambda (msg)
         (if animated?
             ;(animated-dialog msg game-width #:skip spd)
             ;(new-sprite (draw-dialog-text msg game-width))
             (fast-animated-dialog msg game-width #:skip spd)
             (new-sprite (format-dialog msg game-width) #:color 'yellow)
             )
         ) dialog-list))


; === DIALOG RULES ===
(define (all-dialog-closed? g e)
  (and (not (get-entity "player dialog" g))
       (not (get-entity "npc dialog" g))
       (not (get-entity "crafting list" g))))

(define (npc-spoke-and-near? name)
  (lambda (g e)
    (if (and (get-entity "npc dialog" g)
             ((near? name) g e))
        #t
        #f)))

(define (player-spoke-and-near? name)
  (lambda (g e)
    (if (and (get-entity "player dialog" g)
             ((near? name) g e))
        #t
        #f)))

(define (ready-to-speak-and-near? name)
  (lambda (g e)
    (and (not (get-entity "player dialog" g))
         (not (get-entity "dialog bg" g))
         (not (get-entity "npc dialog" g))
         (get-entity name g)
         (not (get-component (get-entity name g) disabled?))
         ((near? name) g e))))

(define (npc-dialog-open? g e)
  (get-entity "npc dialog" g))

(define (player-dialog-open? g e)
  (and (get-entity "player dialog" g)
       ))

(define (get-dialog-length dialog)
  (length dialog))

(define (simple-dialog? dialog)
  (animated-sprite? (first dialog)))

(define last-dialog?
  (lambda (g e)
    (define npc-dialog-entity (get-entity "npc dialog" g))
    (if npc-dialog-entity
        (let ([npc-dialog-index (get-dialog-index npc-dialog-entity)]
              [player-dialog-index (get-counter (get-entity "player" g))]
              [dialog-sprites (get-dialog-sprites npc-dialog-entity)])
          (if (simple-dialog? dialog-sprites)
              (= npc-dialog-index (get-dialog-length dialog-sprites))
              (= npc-dialog-index (get-dialog-length (list-ref dialog-sprites player-dialog-index)))))
        #f
        )))

(define not-last-dialog?
  (lambda (g e)
    (define npc-dialog-index (get-dialog-index (get-entity "npc dialog" g)))
    (define player-dialog-index (get-counter (get-entity "player" g)))
    (define dialog-sprites (get-dialog-sprites (get-entity "npc dialog" g)))
    (define simple-dialog? (animated-sprite? (first dialog-sprites)))
    (define dialog-length (if simple-dialog?
                              (length dialog-sprites)
                              (length (list-ref dialog-sprites player-dialog-index))))
    (if (and ;(get-entity "npc dialog" g)
             ;((near? "player") g e)
             (not (= npc-dialog-index dialog-length)))
        #t
        #f)))