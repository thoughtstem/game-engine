#lang racket

(provide spawner
         update:spawner/to-spawn
         update:spawner/to-spawn^
         spawner-to-spawn
         spawner?
         dead
         dead?)

(require "./define-component.rkt")

(define-component spawner (to-spawn))
(define-component dead ())
