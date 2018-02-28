#lang racket

#;(

(provide rpg-sprite
         rpg-animator)

(require "../../game-engine.rkt")

(define rpg-sheet (bitmap/url "https://orig00.deviantart.net/9b3c/f/2009/193/f/2/blank_sprite_sheet_4_2_by_knightyamato.png"))

(define (rpg-sprite-row r)
  (sheet->sprite rpg-sheet
                 #:rows        8
                 #:columns     12
                 #:row-number  r
                 #:speed       1))

(define rpg-sprite (rpg-sprite-row 1))

(define (rpg-animator dir)
  (define r
    (match dir
      ['none 3]
      ['left 2]
      ['right 3]
      ['up 4]
      ['down 1]))
  (rpg-sprite-row r))

(test-character rpg-animator)

)