#lang racket

;TODO:  These meta-component semantics requires that their child components have update functions.  Maybe that makes sense, but just need to think about it and do a contract error if someone passes in a child wihtout an update.  Clarify this.  See note in core/spawner.rkt...

(provide (rename-out [default-after-ticks after-ticks]))

(require "../../core/main.rkt")

(define-component after-ticks (counter c))

(define (when-counter=0 h g e c)
  (cond
    [(= (after-ticks-counter c) 0) 
     (h g e c)]
    [else  
      ((update:after-ticks/counter^ sub1) g e c) ]))

(define (behave-like-child-on g e c)
  (define fake-c (after-ticks-c c))
  (define fake-e (update-component e c fake-c))

  (define h (component-update fake-c))

  (when (not h)
    (displayln
      "TODO: There wasn't a handler attached to the child component of after-ticks.  Do we need to catch this?"))


  (define fake-ret (h g fake-e fake-c))
  
  (update-component fake-ret fake-c c))

(define (update-after-ticks g e c)
  (when-counter=0 behave-like-child-on g e c))

(define (default-after-ticks n c)
  (after-ticks n c #:update update-after-ticks))


