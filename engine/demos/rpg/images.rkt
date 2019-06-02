#lang racket

(require "../../main.rkt")

(provide elves-up elves-right elves-left elves-down bg-sprite)

(require 2htdp/image)   

(define elf (bitmap "./images/darkelf-sheet.png"))
(define elves-up    (sheet->list elf #:row 3))
(define elves-right (sheet->list elf #:row 2))
(define elves-left  (sheet->list elf #:row 1))
(define elves-down  (sheet->list elf #:row 0))

(require racket/runtime-path)

(define-runtime-path here ".")

(define bg-sprite 
  (register-sprite (build-path here "../images/forest-bg.png" )))

