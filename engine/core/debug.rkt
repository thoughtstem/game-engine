#lang racket

(provide debug current-game step-current-game
         (rename-out [step-current-game step]))

(require "./printer.rkt"
         "./base.rkt")

;The vision is that a user can step through the various runtime steps, always able to get a clear picture of how their state is evolving.  That means dumping the state to text or rendering it to an image, or projecting it to a new game and dumping to text or rendering as an image.  Projections are so meta... 

;Seems like what I really want is some way to step to a debug point, and get control back on my repl.  Then I can run queries on the "current game state", a debug-specific vocabulary word.   Some querie might pop open an image or manipulate a (paused?) rendering of the current game state.


;How can I get control back from an arbitrary point?
;   Raise a special exception and catch it at the top, dumping certain things...
;   That seems pretty fucking good actually...
;     (debug ...) can contain the catching logic
;   All the (debug:* ...) can do the throwing

;We don't need to be rendering during debug.  Just able to render on demand.  What do we render?  Whatever games are appropriate.  Maybe the current game state gets projected.  Maybe some other extra-game state, like the spawn queue gets turned into a game and rendered (or even manipulated in that side-game, and the result is converted back into the actual current spawn queue).   Being able to mutate these debug state dumps seems like a key to exploring the system interactively...

;What's the simplest possible next step to explore this Big Idea?

(define-struct (debug-stop exn:fail:user)  ())


(define-syntax-rule (define-debug-hook (name param ...)
                                       body ...)
  (begin
    (provide name)
    (define (name param ...)
      (when (debug-mode)
        (blue-display "[begin] " 'name)
        body ...
        (blue-display "[end] " 'name)
        (set! current-game (first (list param ...)))
        (save-it! 'name) ))))


(define-syntax-rule (debug exp ...)
  (with-handlers ([debug-stop? (lambda (e)
                                 (blue-display "!!!!!!!!!!!!!!!!") 
                                 (blue-display "!!!          !!!") 
                                 (blue-display "!!!DEBUG STOP!!!") 
                                 (blue-display "!!!          !!!") 
                                 (blue-display "!!!!!!!!!!!!!!!!") 
                                 (raise e)
                                 )])

  (parameterize ([debug-mode #t]) 
    exp ...)))

(define (save-it! hook-name)
  (define do-exception #t)

  (call-with-composable-continuation
    (lambda (k) ; k is the captured continuation

      (set! next-step 
        (lambda ()
          (set! do-exception #f)
          k)) 
      0))
  
  (when do-exception
    (raise (debug-stop (~a 'name) (current-continuation-marks)))))

(define current-game #f)
(define next-step #f)

(define (step-current-game)
  ((next-step)))


(define-debug-hook (debug:tick-begin g)
    (blue-display (~a "********TICK BEGIN*******"))

    (pretty-print-game g)  
    )

(define-debug-hook (debug:entity-tick-begin e)
      (blue-display "****ENTITY TICK BEGIN****")
      (pretty-print-entity e)  
      )


(define-debug-hook (debug:component-tick-begin c)
     (blue-display "****Ticking Component****")
     (pretty-print-component c)  
     )

(define-debug-hook (debug:component-tick-end c)
     (blue-display "****Done Ticking Component****")
     (pretty-print-component c) )



(define-debug-hook (debug:entity-mid-tick-begin e)
   (blue-display "***Mid-Tick Entity***")
   (pretty-print-entity e))


;TODO: This one needs to get justified.  What happens in the mutable state versus the non-mutable state.  WE need to document this and explain it.  It's all fuzzy now...
(define-debug-hook (debug:applying-op op)
    (blue-display "****Applying op****")
    (pretty-print-entity op))


;Adding what to spawn queue?  The mid-tick entity?  The op?  What's the diff?
;  Figure it out and reflect this in the name.
(define-debug-hook (debug:added-to-spawn-queue q)
  (green-display "****Added to spawn queue****")
  (map pretty-print-entity q))

(define-debug-hook (debug:added-to-remove-queue q)
  (red-display "****Added to remove queue****")
  (map pretty-print-entity q))


(define-debug-hook (debug:stripped-spawner-from-entity e)
  (green-display "****Stripped spawner from entity****")
  (pretty-print-entity e))

(define-debug-hook (debug:all-entities-ticked g)
  (blue-display "****All entities ticked****")
  (pretty-print-game g))

(define-debug-hook (debug:removing-entity e)
  (red-display "***REMOVING ENTITY***")
  (pretty-print-entity e))

(define-debug-hook (debug:spawning-entity e)
  (green-display "***SPAWNING ENTITY***")
  (pretty-print-entity e))

(define-debug-hook (debug:check-component c)
  (green-display "*** Check component ***")
  (pretty-print-component c))


(define-debug-hook (debug:check-entity c)
  (green-display "*** Check entity ***")
  (pretty-print-entity c))

(define-debug-hook (debug:check-game g)
  (green-display "*** Check game ***")
  (pretty-print-game g))

(define-debug-hook (debug:check-all g e c)
  (green-display "*** Check all ***")
  (green-display "*** Game ***")
  (pretty-print-game g)
  (green-display "*** Entity ***")
  (pretty-print-entity e)
  (green-display "*** Component ***")
  (pretty-print-component c))



(define-debug-hook (debug:after-entity-update g e c)
  (yellow-display "*** After Entity Update ***")
  (green-display "*** Game ***")
  (pretty-print-game g)
  (green-display "*** Entity ***")
  (pretty-print-entity e)
  (green-display "*** Component ***")
  (pretty-print-component c))



(define-debug-hook (debug:after-removals g q)
  (red-display "*** Removing ***")
  (map pretty-print-entity q)

  (red-display "*** After removals ***")
  (pretty-print-game g))

(define-debug-hook (debug:after-spawns g q)
  (green-display "*** Spawning ***")
  (map pretty-print-entity q)

  (green-display "*** After spawns ***")
  (pretty-print-game g))

