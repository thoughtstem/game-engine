#lang racket

(provide elf-animation hero-animation)

(require "../../main.rkt"
         "./images.rkt")

(define (hero-animation (which-animation elf-animation))
  (which-animation
    #:direction-update
    (thunk* (get-input-direction)))) 

(define (elf-animation #:direction-update (dir-up identity))
  (animation-system #:direction-update dir-up 
                    elves-up elves-right elves-down elves-left))
