#lang racket

(provide id-generator)

(define (id-generator start)
  (define TEMP 0)
  (lambda ()
    (set! TEMP (add1 TEMP))
    TEMP))
