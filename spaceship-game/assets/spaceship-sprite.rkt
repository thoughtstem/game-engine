#lang racket

(provide spaceship-sprite
         spaceship-animator)

(require "../../game-engine.rkt")

(define spaceship-sheet (bitmap/url "http://i.imgur.com/8zY5sBR.png"))
(define spaceship-sprite
  (sheet->sprite spaceship-sheet
                 #:rows        4
                 #:columns     3
                 #:row-number  3
                 #:speed       1))


(define (spaceship-animator dir)
  (define r
    (match dir
      ['none 3]
      ['left 2]
      ['right 3]
      ['up 4]
      ['down 1]))
  (sheet->sprite spaceship-sheet
                 #:rows        4
                 #:columns     3
                 #:row-number  r
                 #:speed       1))


;(test-character spaceship-animator)