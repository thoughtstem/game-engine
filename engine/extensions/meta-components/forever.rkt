#lang racket

(provide forever?
         (rename-out [default-forever forever]))

(require "../../core/main.rkt")

(define-component forever (child original))

(define (behave-like-current-child-on g e c)
  ;Find current child, mimic, detect removal, reset and do the extra tick as if it never removed itself...

  (define fake-c (forever-child c))
  (define fake-e (update-component e c fake-c))  ;What if mutable???
  (define h (component-update fake-c)) 

  ;I don't understand what the ramifications are for when 
  ; we run h and we are in mutable! mode.  Need to get a better
  ; handle on that.  Can we even do this kind of "lookahead"?
  (define new-fake-e (h g fake-e fake-c)) 
  (define ticked-fake-c (get-component new-fake-e fake-c))  

  (define new-c
    (if (not ticked-fake-c) ;It removed itself  
      (update:forever/child c (forever-original c))  

      (update:forever/child c ticked-fake-c)))

  (if (not ticked-fake-c) ;It removed itself
    ;Tick it once to remove the ticks where the forever component is waiting for the next child to start.
    ;TODO: I think there's an infinite loop in this branch...

    #;
    (tick-component g (add-component new-fake-e new-c) new-c)  

    (add-component e new-c)  ;Put self back on.  There will be an empty tick where the forever "reboots", maybe we don't want this?
    (update-component new-fake-e fake-c new-c)))


(define (update-forever g e c)
  (behave-like-current-child-on g e c))

;Takes in a child that and behaves like that child until it tries to remove itself, then resets the child to its original values and continues to mimic the child.
(define (default-forever c)
  ;Should it copy the cs into these two slots?
  (forever (copy-component c) c #:update update-forever))



