#lang racket

(provide counter get-counter
         number-stream get-number-stream
         )
(require "../../core/main.rkt")

(define-component counter number?)

(define-component number-stream stream?)
