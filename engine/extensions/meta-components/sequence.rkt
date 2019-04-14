#lang racket

(provide sequence?
         (rename-out 
           [ default-sequence sequence]))

(require "../../core/main.rkt")

(define-component sequence (children))

(define (set-first l c)
  (list-set l 0 c))

(define (until-children-empty h g e c)
  (cond
    [(empty? (sequence-children c))
     (remove-component e c)]
    [else  
      (h g e c)]))

(define (behave-like-current-child-on g e c)
  ;Find current child, mimic, detect removal, remove from children list...

  (define fake-c (first (sequence-children c)))
  (define fake-e (update-component e c fake-c))
  (define h (component-update fake-c)) 

  (define fake-ret (h g fake-e fake-c))
  (define ticked-fake-c (get-component fake-ret fake-c))  

  (define new-c
    (if (not ticked-fake-c) ;It removed itself  
      (update:sequence/children c rest)  
      (update:sequence/children c (curryr set-first ticked-fake-c))))  

  (if (not ticked-fake-c) ;It removed itself
    ;Tick it once to remove the ticks where the sequence component is waiting for the next child to start.
    (tick-component g
                    (add-component fake-ret new-c)
                    new-c)  

    (update-component fake-ret fake-c new-c)))


(define (update-sequence g e c)
  (until-children-empty behave-like-current-child-on g e c))

;Takes in a for-ticks component, behaves like that component until it tries to remove itself, then resets that component-s ticks back to its original value and runs it again.  Does so n sequence.
(define (default-sequence . cs)
  (sequence cs #:update update-sequence))
