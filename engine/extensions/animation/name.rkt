#lang racket

(require "../../core/main.rkt")

(provide name get-name has-name)
(define-component name symbol?)

(define (has-name s)
  (lambda(e)
    (eq? 'input-manager (get-name e))))
