#lang racket

(require "../main.rkt")

(require 2htdp/image)   

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

(define elf (bitmap "./darkelf-sheet.png"))
(define elves-right (sheet->list elf #:row 2))
(define elves-left  (sheet->list elf #:row 1))
(define elves-down  (sheet->list elf #:row 0))
      
(define-component counter number?)                                         
(define-component direction posn?)                                         
(define-component facing symbol?)                                         

(define-component animation-system entity?)                                         

;Cool.  What's the next abstraction step?
(define (make-animation-system left-frames right-frames down-frames) 
  (list
    (animation-system
      (entity ;Encapsulates an entity whose sprite animation swaps based on specific user input.
        (counter 0 (^ add1))
        (direction #f (as-posn (get-current-input)))

        (facing 'left
                (cond 
                  [(> 0 (posn-x (get-direction)))
                   'left ]
                  [(< 0 (posn-x (get-direction)))
                   'right]
                  [else 'down]))

        (sprite (first elves-left) 
                (list-ref 
                  (cond 
                    [(eq? 'right (get-facing))
                     right-frames]
                    [(eq? 'left (get-facing))
                     left-frames]
                    [(eq? 'down (get-facing))
                     down-frames])
                  (if (equal? (posn 0 0) (get-direction))
                    0  
                    (remainder (get-counter) 
                               (length left-frames))))))   
      (^ tick-entity)) 

    (sprite (first left-frames)
            (get-sprite (get-animation-system)))))



(define e
  (entity
    ;Could abstract into a "movement-system"
    (direction #f (as-posn (get-current-input)))
    (position (posn 200 200)
              (posn-add 
                (get-position)
                (get-direction)))

    ;How to ensure that systems compose with each other?  Some kind of contract system or types?
    ;(Are we going down a rabit hole?  Check the docs for todos before addressing any of these.)
    (make-animation-system elves-left elves-right elves-down)))

(play! (game input-manager e))   



