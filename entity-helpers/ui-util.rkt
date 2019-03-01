#lang racket

(provide bordered-box-sprite)

(require "../game-entities.rkt")
(require "../components/animated-sprite.rkt")

(require 2htdp/image)

(define (bordered-box-sprite w h)
  (define outer-border-img (square 1 'solid 'black))
  (define inner-border-img (square 1 'solid 'white))
  (define box-img (square 1 'solid 'dimgray))

  (precompile! outer-border-img
               inner-border-img
               box-img)
  
  (list (new-sprite  box-img
                     #:animate #f
                     #:x-scale (- w 6)
                     #:y-scale (- h 6))
        (new-sprite inner-border-img
                    #:animate #f
                    #:x-scale (- w 2)
                    #:y-scale (- h 2))
        (new-sprite outer-border-img
                    #:animate #f
                    #:x-scale w
                    #:y-scale h)
        ))