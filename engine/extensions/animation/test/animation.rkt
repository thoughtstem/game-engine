#lang racket

(require "../main.rkt"
         "../animation-system.rkt"
         "../movement-system.rkt")

(require 2htdp/image)   

(define elf (bitmap "./images/darkelf-sheet.png"))
(define elves-right (sheet->list elf #:row 2))
(define elves-left  (sheet->list elf #:row 1))
(define elves-down  (sheet->list elf #:row 0))

(define e
  (entity
    (movement-system)
    (animation-system 
      #:direction-update (thunk* (as-posn (get-current-input))) 
      elves-left elves-right elves-down)))

(play! (game ( input-manager) e))   



