#lang racket

(provide animated-dialog
         pad
         draw-title
         draw-dialog
         draw-dialog-text
         draw-dialog-sheet
         draw-dialog-sheet-text
         draw-dialog-list
         dialog->sprites
         dialog->response-sprites
         next-dialog
         next-response
         next-dialog-option
         previous-dialog-option
         reached-frame?
         create-dialog
         dialog-lg
         dialog-list
         dialog-selection
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
         get-selection-offset
         )

(require "../game-entities.rkt")
(require "../components/animated-sprite.rkt")
(require "../components/counter.rkt")
(require "../components/spawn-dialog.rkt")
(require "../components/do-every.rkt")
(require "../components/after-time.rkt")
(require "../components/lock-to.rkt")
(require "../components/on-key.rkt")
(require "../components/on-start.rkt")
(require "../components/on-rule.rkt")
(require "../components/sound-stream.rkt")
(require "../components/dialog.rkt")
(require "../component-util.rkt")
(require "../entity-helpers/movement-util.rkt")
(require "../entity-helpers/sprite-util.rkt")
(require "../entity-helpers/rgb-hsb.rkt")

(require 2htdp/image
         posn)


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

(define (draw-title msg)
  (define message (text msg 24 "yellow"))
  (overlay message
           (rectangle (+ 12 (image-width message)) (+ 12 (image-height message)) "outline" (pen "white" 2 "solid" "butt" "bevel"))
           (rectangle (+ 16 (image-width message)) (+ 16 (image-height message)) "solid" (make-color 20 20 20 150))))

(define (draw-dialog msg)
  (define message (text msg 12 "yellow"))
  (overlay message
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


(define (next-dialog dialog-list #:sound [rsound #f])
  (lambda (g e)
    (define WIDTH (game-width g))
    (define HEIGHT (game-height g))
    (define dialog-index (get-counter e))
    ;(displayln (~a "CURRENT DIALOG: " dialog-index))
    (define dialog-length (length dialog-list))
    (define name (get-name e))
    (define avatar-img (pick-frame-original (get-component e animated-sprite?) 0))
    (define avatar-box
      (freeze (overlay (rectangle 56 56 "outline" (pen "white" 2 "solid" "butt" "bevel"))
                       (rectangle 58 58 "outline" (pen "black" 2 "solid" "butt" "bevel"))
                       (place-image
                        (freeze (scale-to-fit avatar-img 80))
                        20 40
                        (rectangle 60 60 "solid" (make-color 255 255 255 100))))))
    (define message-entity (create-dialog dialog-list name (posn (/ (* WIDTH 2.5) 4) (- HEIGHT 40)) #:sound rsound))
    (update-entity (add-component e
                                  (spawn-dialog (dialog-lg avatar-box name message-entity WIDTH #:delay 5)))
                   counter?
                   (counter (add1 dialog-index)))))

(define (next-response response-list #:sound [rsound #f])
  (lambda (g e)
    (define WIDTH (game-width g))
    (define HEIGHT (game-height g))
    (define player-dialog-index (get-counter (get-entity "player" g)))
    (define npc-dialog-index (get-counter e))
    (define response-length (length (list-ref response-list player-dialog-index)))
    (define name (get-name e))
    (define avatar-img (pick-frame-original (get-component e animated-sprite?) 0))
    (define avatar-box
      (freeze (overlay (rectangle 56 56 "outline" (pen "white" 2 "solid" "butt" "bevel"))
                       (rectangle 58 58 "outline" (pen "black" 2 "solid" "butt" "bevel"))
                       (place-image
                        (freeze (scale 2 avatar-img))
                        30 30
                        (rectangle 60 60 "solid" (make-color 255 255 255 100))))))
    (define message-entity (create-dialog response-list name (posn (/ (* WIDTH 2.5) 4) (- HEIGHT 40)) #:sound rsound))
    (add-component (update-entity e counter? (counter (add1 npc-dialog-index)))
                   (spawn-dialog (dialog-lg avatar-box name message-entity WIDTH #:delay 10)))))


(define (next-dialog-option dialog-list box-height)
  (lambda (g e)
    (define new-index (modulo (add1 (get-counter e)) (length dialog-list)))
    (define offset (posn 0 (get-selection-offset (length dialog-list) box-height new-index)))
    (update-entity (update-entity e lock-to? (lock-to "player dialog" #:offset offset))
                   counter?
                   (counter new-index))))

(define (previous-dialog-option dialog-list box-height)
  (lambda (g e)
    (define new-index (modulo (sub1 (get-counter e)) (length dialog-list)))
    (define offset (posn 0 (get-selection-offset (length dialog-list) box-height new-index)))
    (update-entity (update-entity e lock-to? (lock-to "player dialog" #:offset offset))
                   counter?
                   (counter new-index))))

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
    (define dialog-sprite (get-component e animated-sprite?))
    ((change-sprite (new-sprite (pick-frame dialog-sprite (sub1 (animated-sprite-total-frames dialog-sprite))) 1)) g e)))
    
(define (create-dialog dialog-list name pos #:delay [delay-time 0] #:sound [rsound #f])
  (sprite->entity empty-image ;(draw-dialog msg)
                  #:name       "npc dialog"
                  #:position   pos ;(posn 0 -40)
                  #:components (static)
                               (hidden)
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
     
(define (dialog-lg avatar name message-entity game-width #:delay [delay-time 0])
  (define padded-name (~a name
                          #:min-width 18
                          #:max-width 80
                          #:limit-marker "..."
                          #:align 'center))
  (sprite->entity (above/align "left"
                               (draw-dialog padded-name)
                               (place-image/align (pad avatar 16 16)
                                                  0 40 "left" "center"
                                                  (overlay (rectangle (- game-width 4) 76 "outline" (pen "white" 2 "solid" "butt" "bevel"))
                                                           (rectangle game-width       80 "solid"  (make-color 20 20 20 150)))))
                  #:name       "dialog bg"
                  #:position   (posn 0 0)
                  #:components (static)
                               (hidden)
                               ;(on-key 'space die)
                               (on-key 'enter #:rule last-dialog? die)
                               (on-start (go-to-pos-inside 'bottom-center))
                               (after-time delay-time (do-many show
                                                               (open-dialog message-entity)))
                  ))


(define (dialog-list dialog-list pos #:sound [rsound #f])
  (define selection 0)
  (define font-size 18)
  (define dialog-list-sprite (draw-dialog-list dialog-list font-size selection))
  (sprite->entity dialog-list-sprite
                  #:name       "player dialog"
                  #:position   pos
                  #:components (static)
                               (hidden)
                               (on-start (do-many (go-to-pos 'center)
                                                  show
                                                  (open-dialog (dialog-selection dialog-list
                                                                                 (image-width dialog-list-sprite)
                                                                                 font-size
                                                                                 selection
                                                                                 rsound))))
                               (on-key 'enter die)
                               ))

(define (get-selection-offset max-options box-height selection)
  (* (- selection (/ (sub1 max-options) 2)) box-height))

(define (dialog-selection dialog-list max-width font-size selection rsound)
  (define select-box
    (overlay (rectangle (- max-width 14)
                        (+ 4 (image-height (text "Blank" font-size "transparent")))
                        "outline"
                        (pen "white" 2 "solid" "butt" "bevel"))
             (rectangle (- max-width 10)
                        (+ 8 (image-height (text "Blank" font-size "transparent")))
                        "outline"
                        (pen "black" 4 "solid" "butt" "bevel"))))
  (define box-height (image-height select-box))
  (define offset (posn 0 (get-selection-offset (length dialog-list) box-height selection)))
  (sprite->entity select-box
                  #:name       "player dialog selection"
                  #:position   (posn 0 0) ;(posn (/ WIDTH 2) (+ (/ HEIGHT 2) (posn-y offset)))
                  #:components (static)
                               (hidden)
                               (counter selection)
                               (on-start show)
                               (lock-to "player dialog" #:offset offset)
                               ;(on-key 'space die)
                               (on-key 'enter die)
                               (on-key 'up   (if rsound
                                                 (do-many (previous-dialog-option dialog-list box-height)
                                                          (play-sound-from "player" rsound))
                                                 (previous-dialog-option dialog-list box-height)))
                               (on-key 'down (if rsound
                                                 (do-many (next-dialog-option dialog-list box-height)
                                                          (play-sound-from "player" rsound))
                                                 (next-dialog-option dialog-list box-height)))))



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
    (define selection (get-counter (get-entity "player dialog selection" g)))
    ;(displayln (~a "Player Selection: " selection))
    (update-entity e counter? (counter selection))))

; === GENERIC SPRITE GENERATORS ===
(define (dialog->response-sprites dialog-list #:game-width game-width #:animated [animated? #t] #:speed [spd 2])
  (map (lambda (response-list)
         (map (lambda (msg)
                (if animated?
                    (animated-dialog msg game-width #:skip spd)
                    (new-sprite (draw-dialog-text msg)))
                ) response-list)) dialog-list))

(define (dialog->sprites dialog-list #:game-width game-width #:animated [animated? #t] #:speed [spd 2])
  (map (lambda (msg)
         (if animated?
             (animated-dialog msg game-width #:skip spd)
             (new-sprite (draw-dialog-text msg)))
         ) dialog-list))


; === DIALOG RULES ===
(define (all-dialog-closed? g e)
  (and (not (get-entity "player dialog" g))
       (not (get-entity "npc dialog" g))
       (not (get-entity "crafting list" g))))

(define (npc-spoke-and-near? name)
  (lambda (g e)
    (if (and (get-entity "npc dialog" g)
             ((near-entity? name) g e))
        #t
        #f)))

(define (player-spoke-and-near? name)
  (lambda (g e)
    (if (and (get-entity "player dialog" g)
             ((near-entity? name) g e))
        #t
        #f)))

(define (ready-to-speak-and-near? name)
  (lambda (g e)
    (and (not (get-entity "player dialog" g))
         (not (get-entity "dialog bg" g))
         (not (get-entity "npc dialog" g))
         (get-entity name g)
         (not (get-component (get-entity name g) disabled?))
         ((near-entity? name) g e))))

(define (npc-dialog-open? g e)
  (get-entity "npc dialog" g))

(define (player-dialog-open? g e)
  (and (get-entity "player dialog" g)
       (get-entity "player dialog selection" g)))

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
             ;((near-entity? "player") g e)
             (not (= npc-dialog-index dialog-length)))
        #t
        #f)))