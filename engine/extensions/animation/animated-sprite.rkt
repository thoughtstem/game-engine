#lang racket

(provide 
  sprite
  sprite?
  sprite-id
  get-sprite
  register-sprite
  
  x
  y

  position
  get-position
  position?

  rotation
  get-rotation

  size
  get-size

  get-queued-sprites
  flush-queued-sprites!
  
  move-to

  set-insertion-queue!
  set-seen-sprite-ids! 

  sheet->list

  also-render
  get-also-render

  also-render
  get-also-render
  )

(require "../../core/main.rkt"
         "./common-components.rkt"
         (only-in 2htdp/image bitmap/file image->color-list crop image-width image-height)
         posn)

(define-component position posn?)
(define-component rotation number?)
(define-component size number?)                                         


(define (x e)
  (posn-x (get-position e)))

(define (y e)
  (posn-y (get-position e)))

(define (move-to p e)

  (define current-p 
    (get-component e 'position))

  (define new-p
    (set-position current-p p))

  (update-component e 
                    current-p
                    new-p))

;A component that wraps a single sprite id
;  there's no animation at this component's level.
;  It can be used to create animation systems with more complex
;  components.
(define-component also-render game?)

(define-component sprite symbol?)

(define (sprite-id s)
  (get-sprite s))

;Whenever you construct a new sprite, it ends up in the
; insertion queue, along with its id.  This is the last step
; before we throw the image away (to the gcard) and refer to it only by id.
(define insertion-queue '())  ;Holds images, Gets emptied
(define seen-sprite-ids '())  ;Doesn't hold images, Doesn't get emptied

(define (seen? i)
  (member i seen-sprite-ids))


(define (image->id i)
  (string->symbol
    (~a "sprite-"
        (equal-hash-code (~a (image->color-list i))))))


(define (register-sprite i)
  (let ([id 
          (if (path? i)     
            (string->symbol (~a "sprite-" i))
            (image->id i))]
        [final-image
          (if (path? i)
            (bitmap/file i) 
            i)])

    (when (not (seen? id))
      (set-insertion-queue! (cons (list id final-image) insertion-queue))
      (set-seen-sprite-ids! (cons id seen-sprite-ids)))

    id))

(define (set-insertion-queue! l)
  (set! insertion-queue l))

(define (set-seen-sprite-ids! l)
  (set! seen-sprite-ids l))

(define (get-queued-sprites) insertion-queue)
(define (flush-queued-sprites!) 
  (set! insertion-queue '()))


(define (sheet->list i #:row (r 0))
  (define ew (image-width i))
  (define eh (image-height i))

  (define rows 4)
  (define cols 4)

  (define cw  (/ ew cols))
  (define ch  (/ eh rows))

  (define elf1 (register-sprite (crop (* cw 0) (* ch r) cw ch i)))
  (define elf2 (register-sprite (crop (* cw 1) (* ch r) cw ch i)))
  (define elf3 (register-sprite (crop (* cw 2) (* ch r) cw ch i)))
  (define elf4 (register-sprite (crop (* cw 3) (* ch r) cw ch i)))

  (define elves (list elf1 elf2 elf3 elf4))

  elves)

