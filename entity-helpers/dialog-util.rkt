#lang racket

(provide animated-dialog
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
         dialog
         dialog-lg
         dialog-list
         dialog-selection
         start-blips
         stop-blips
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
(require "../component-util.rkt")
(require "../entity-helpers/movement-util.rkt")
(require "../entity-helpers/sprite-util.rkt")

(require 2htdp/image
         posn)


(define (pad image w-pad h-pad)
  (define w (image-width image))
  (define h (image-height image))
  (define w-padding (* w-pad 2))
  (define h-padding (* h-pad 2))
  (overlay image
           (rectangle (+ w w-padding) (+ h h-padding) "solid" "transparent")))

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
    (define name (get-name e))
    (define avatar-img (pick-frame-original (get-component e animated-sprite?) 0))
    (define avatar-box
      (freeze (overlay (rectangle 56 56 "outline" (pen "white" 2 "solid" "butt" "bevel"))
                       (rectangle 58 58 "outline" (pen "black" 2 "solid" "butt" "bevel"))
                       (place-image
                        (freeze (scale 2 avatar-img))
                        30 30
                        (rectangle 60 60 "solid" (make-color 20 20 20 150))))))
    (define message-entity (dialog (list-ref dialog-list dialog-index) name (posn (/ (* WIDTH 2.5) 4) (- HEIGHT 40)) #:sound rsound))
    (update-entity (add-component e
                                  (spawn-dialog (dialog-lg avatar-box name message-entity WIDTH #:delay 5)))
                   counter?
                   (counter (modulo (add1 dialog-index) (length dialog-list))))))

(define (next-response response-list #:sound [rsound #f])
  (lambda (g e)
    (define WIDTH (game-width g))
    (define HEIGHT (game-height g))
    (define dialog-index (get-counter (get-entity "player dialog selection" g)))
    (define name (get-name e))
    (define avatar-img (pick-frame-original (get-component e animated-sprite?) 0))
    (define avatar-box
      (freeze (overlay (rectangle 56 56 "outline" (pen "white" 2 "solid" "butt" "bevel"))
                       (rectangle 58 58 "outline" (pen "black" 2 "solid" "butt" "bevel"))
                       (place-image
                        (freeze (scale 2 avatar-img))
                        30 30
                        (rectangle 60 60 "solid" (make-color 20 20 20 150))))))
    (define message-entity (dialog (first (shuffle (list-ref response-list dialog-index))) name (posn (/ (* WIDTH 2.5) 4) (- HEIGHT 40)) #:sound rsound))
    (add-component e
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

(define (reached-frame? end-frame)
  (lambda (g e)
    (define as (get-component e animated-sprite?))
    (define total-frames (animated-sprite-total-frames as))
    (define current-frame (animated-sprite-current-frame as))
    (if (> total-frames 1)
        (= current-frame end-frame)
        #f)))


(define (dialog dialog-sprite name pos #:delay [delay-time 0] #:sound [rsound #f])
  (sprite->entity empty-image ;(draw-dialog msg)
                  #:name       "npc dialog"
                  #:position   pos ;(posn 0 -40)
                  #:components (static)
                               (hidden)
                               ;(on-key 'space die)
                               (on-key 'enter die)
                               ;(on-start (go-to (/ (* WIDTH 2.5) 4) (- HEIGHT 40)))
                               (on-rule (reached-frame? (sub1 (animated-sprite-total-frames dialog-sprite))) (do-many (change-sprite (new-sprite (pick-frame dialog-sprite (sub1 (animated-sprite-total-frames dialog-sprite))) 1))
                                                                                                                      (stop-blips)))
                               (after-time delay-time (if rsound
                                                          (do-many (change-sprite dialog-sprite)
                                                                   show
                                                                   (start-blips name rsound))
                                                          (do-many (change-sprite dialog-sprite)
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
                               (on-key 'enter die)
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