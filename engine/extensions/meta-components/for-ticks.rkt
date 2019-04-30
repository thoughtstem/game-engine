#lang racket

(provide for-ticks)

(require "../../core/main.rkt")

(define (for-ticks n)

  (define counter 0)

  (lambda (g e c)
    (set! counter (add1 counter))
    (if (= counter n)
      (begin
        (remove-component e c)) 
      e)))

#;
(provide (rename-out [ default-for-ticks for-ticks]))


;Here's a natural implemention of for-ticks.  But I feel like it doesn't quite work...
;
;First off, when components are copied, what happens to the update function reference and do we need to care?
;  Unless I'm missing something this would cause any spawned entity with a for-ticks component to update correctly, but faster than expected if there are multiple spawns...
;  Maybe we should verify this behaviour -- if we're wrong and it works, then this is a nice and simple solution.  But if we're right...
;  We may have to put the counter into a for-ticks component instead of using new-component.
;  Or use a component-system, which might have one (counter ...) component, and an additional component that does the same logic as below, but references the counter instead of the ticks-since-spawn variable


;I'm also having trouble reasoning about components storing references to other components -- e.g. for later removal.  If we store the component id, then don't we run into trouble with uniqueness?    
;  (Easier to see in the case where you need to spawn something twice and then remove both... but is that realistic?)
;  (Or even in this case if two for-ticks were both given the same other component, one would might remove the other's)
;Does the problem go away if the crud op for adding a component uniquifies the id and then also tells the caller how it changed it?
;  Then anyone who does spawning can just (if it cares) get that ref back and store it somewhere...

;And... This is reminding me of running into troubles with (forever ...), which required restarting.  Maybe we just ignore (forever ...)
; Was there trouble with (sequence ...) too? 

#;
(define (for-ticks n to-remove)
  ;We'll do the add and remove trick.

  (define ticks-since-spawn 0)

  (new-component #:update
                 (lambda (g e for-ticks-component)
                   (when (my-first-tick)
                     ;Add and tick to make up for the fact that the the to-remove component wasn't on the entity already.  You kind of expect for-ticks to be "for n ticks, starting now."  But maybe that's just me.
                     (add-and-tick e to-remove))

                   (when (my-last-tick)
                     (remove-component e to-remove))

                   (set! ticks-since-spawn (add1 ticks-since-spawn))
                   )))
