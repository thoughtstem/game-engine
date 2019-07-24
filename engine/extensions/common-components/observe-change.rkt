#lang racket

(provide observe-change)

(require "../../core/main.rkt")

(define-syntax-rule (observe-change f)
  (observe-change-f (thunk* f)))

(define (observe-change-f f)
  (define last-value (void)) 

  (thunk*
    (define current-value (f)) 

    (define did-change 
      (not (eq? last-value current-value)))

    (set! last-value current-value)

    did-change))
