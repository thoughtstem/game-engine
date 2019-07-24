#lang racket

(provide death get-death)
(require "../../core/main.rkt")

(define-component death (or/c boolean? despawn?))

