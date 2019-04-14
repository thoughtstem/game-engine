#lang racket

(provide (rename-out [ default-times times]))

(require "../../core/main.rkt"
         "./sequence.rkt")

(define (default-times n c)
  (apply sequence (map (const c) (range n))))
