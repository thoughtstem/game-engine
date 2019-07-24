#lang racket

(provide input-manager
         get-current-input  
         get-input
         as-posn
         get-input-direction)

(require "../../core/main.rkt")

(require 
  "../rendering/renderer.rkt"
  "../common-components/main.rkt")

(define-component input posn?)

(define (get-input-direction)
  (as-posn (get-current-input)))

(define (as-posn i)
  i)

(define (get-current-input)
  (get-input (get-entity (CURRENT-GAME)
                         (has-name 'input-manager))))

(define (input-manager)
  (entity
    (name 'input-manager)
    (input #f 
           (begin
             (posn
               (cond 
                 [(and
                    (hash-ref buttons #\a)
                    (hash-ref buttons #\d))
                  0]
                 [(hash-ref buttons #\a) -1] 
                 [(hash-ref buttons #\d) 1]
                 [else 0])
               (cond 
                 [(and
                    (hash-ref buttons #\w)
                    (hash-ref buttons #\s))
                  0]
                 [(hash-ref buttons #\w) -1] 
                 [(hash-ref buttons #\s) 1]
                 [else 0]) )))))
