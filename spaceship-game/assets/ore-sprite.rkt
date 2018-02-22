#lang racket

(provide ore-sprite)

(require "../../game-engine.rkt")

(define ore-sheet (bitmap/url "http://twicetwo.com/gallery/cgi/powerups.png"))
(define (ore-sprite i)
  (sheet->sprite ore-sheet
                 #:rows        20
                 #:columns     8
                 #:row-number  (+ 1 i)
                 #:speed       1))
