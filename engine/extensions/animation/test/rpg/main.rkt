#lang racket

(require "../../main.rkt")

(require 2htdp/image)   

(define elf (bitmap "../images/darkelf-sheet.png"))
(define elves-right (sheet->list elf #:row 2))
(define elves-left  (sheet->list elf #:row 1))
(define elves-down  (sheet->list elf #:row 0))

(require racket/runtime-path)

(define-runtime-path here ".")

(define bg-sprite 
  (register-sprite (build-path here "../images/forest-bg.png" )))

(define hero
  (entity
    #;
    (movement-system)

    (position (posn 200 200))

    (animation-system elves-left elves-right elves-down)))

(define bg
  (entity
    (movement-system #:direction-update 
                     (thunk* 
                       (posn-scale
                         -1
                         (as-posn
                           (get-current-input)))))

    #;
    (position (posn 0 0))

    (sprite bg-sprite)))

(define rpg
  (game 
    input-manager 
    hero
    bg))     

(play! rpg)


;What to do next in RPG game?
;  What's the game moment?   
;  Decide what kind of game.  What are we making?  Shall we start documenting it?
;  Characters and backgrounds?
;    Backdrop swap?



