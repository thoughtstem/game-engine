#lang racket

(require "../game-entities.rkt")
;(require "../components/after-time.rkt")
(require "./direction.rkt")
(require "./rotation-style.rkt")
(require "./spawn-once.rkt")
(require posn)

;(displayln "LOADING ON START")

(provide spawn-dialog)

(define (spawn-dialog e)
  (spawn-once e #:relative? #f))