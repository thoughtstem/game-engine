#lang racket

(provide (rename-out [ default-for-ticks for-ticks]))

(require "../../core/main.rkt")

(define-component for-ticks (counter c))

(define (until-counter=0 h g e c)
  (cond
    [(= (for-ticks-counter c) 0) 
     (remove-component e c)]
    [else  
      ((compose-handlers
          (update:my/for-ticks/counter^ sub1)
          h)
       g e c)]))

(define (behave-like-child-on g e c)
  (define fake-c (for-ticks-c c))
  (define fake-e (update-component e c fake-c))

  (define h (component-update fake-c))

  (define fake-ret (h g fake-e fake-c))
  
  (update-component fake-ret fake-c c))

(define (update-for-ticks g e c)
  (until-counter=0 behave-like-child-on g e c))

(define (default-for-ticks n c)
  (for-ticks n c #:update update-for-ticks))


