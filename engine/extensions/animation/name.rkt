#lang racket

(require "../../core/main.rkt")

(provide name get-name has-name)
(define-component name symbol?)

(define (has-name s)
  (lambda(e)
    (and
      (has-component e 'name)
      (eq? s (get-name e)))))
