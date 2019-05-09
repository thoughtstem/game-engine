#lang racket

(require "../main.rkt")

(require 2htdp/image)   
        
(define green-star
  (register-sprite (star 40 'solid 'green)))
(define red-star
  (register-sprite (star 40 'solid 'red)))

(define elf (bitmap "./darkelf-sheet.png"))

(define ew 128)
(define eh 192)

(define rows 4)
(define cols 4)

(define cw  (/ ew cols))
(define ch  (/ eh rows))

(define elf1 (register-sprite (crop (* cw 0) (* ch 1) cw ch elf)))
(define elf2 (register-sprite (crop (* cw 1) (* ch 1) cw ch elf)))
(define elf3 (register-sprite (crop (* cw 2) (* ch 1) cw ch elf)))
(define elf4 (register-sprite (crop (* cw 3) (* ch 1) cw ch elf)))

(define elves (list elf1 elf2 elf3 elf4))
      
(define-component counter number?)                                         
    
(define e
  (entity
    (position (posn 200 200)
              (posn-add 
                (get-position)
                (as-posn (get-current-input))))

    (counter 0 (^ add1))

    (sprite elf1 
            (list-ref elves (remainder (get-counter) 4))

            #;
            (if (odd? (get-counter)) 
                      green-star
                      red-star))


    ))
                                                                           
(play! (game input-manager e))   
