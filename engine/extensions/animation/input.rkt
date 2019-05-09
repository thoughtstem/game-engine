#lang racket

(provide input-manager
         get-current-input  
         get-input
         as-posn)

(require "../../core/main.rkt")

(require "./renderer.rkt"
         "./name.rkt")

(define-component input posn?)

(define (as-posn i)
  i)

(define (get-current-input)
  (get-input (get-entity (CURRENT-GAME)
                         (has-name 'input-manager))))

(define input-manager
  (entity
    (name 'input-manager)
    (input #f 
           (begin
             (posn
               (cond 
                 [(and
                    (first buttons)
                    (second buttons))
                  0]
                 [(first buttons) -1] 
                 [(second buttons) 1]
                 [else 0])
               0)))))
