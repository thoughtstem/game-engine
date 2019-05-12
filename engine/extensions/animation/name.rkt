#lang racket

(require "../../core/main.rkt")

(provide name get-name has-name)
(define-component name symbol?)

(define (has-name s)
  (lambda(e)
    (eq? s (get-name e))))
