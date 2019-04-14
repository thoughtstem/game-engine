#lang racket

(provide forever?
         (rename-out 
           [ default-forever forever]))

(require "../../core/main.rkt")

(define-component forever (child original))

(define (behave-like-current-child-on g e c)
  ;Find current child, mimic, detect removal, reset and do the extra tick as if it never removed itself...

  (define fake-c (forever-child c))
  (define fake-e (update-component e c fake-c))
  (define h (component-update fake-c)) 

  (define fake-ret (h g fake-e fake-c))
  (define ticked-fake-c (get-component fake-ret fake-c))  

  (define new-c
    (if (not ticked-fake-c) ;It removed itself  
      (update:forever/child c (forever-original c))  

      (update:forever/child c ticked-fake-c)))

  (if (not ticked-fake-c) ;It removed itself
    ;Tick it once to remove the ticks where the forever component is waiting for the next child to start.
    (tick-component g (add-component fake-ret new-c) new-c)  
    (update-component fake-ret fake-c new-c)))


(define (update-forever g e c)
  (behave-like-current-child-on g e c))

;Takes in a child that and behaves like that child until it tries to remove itself, then resets the child to its original values and continues to mimic the child.
(define (default-forever c)
  (forever c c #:update update-forever))



