#lang racket

(require game-engine)

(require 2htdp/image)   

(define elf (bitmap "./images/darkelf-sheet.png"))
(define elves-right (sheet->list elf #:row 2))
(define elves-left  (sheet->list elf #:row 1))
(define elves-down  (sheet->list elf #:row 0))
(define elves-up  (sheet->list elf #:row 3))

(define direction-from-input
  (thunk* 
    (as-posn (get-current-input))))

(define e
  (entity
    (movement-system 
      #:direction-update direction-from-input)

    (position (posn 200 200)
              (get-position (get-movement-system)))

    (animation-system 
      #:direction-update direction-from-input 
      elves-up elves-right elves-down elves-left )))

(play! 
    (game 
      (input-manager) 
      e))


