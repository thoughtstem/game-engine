#lang racket

(require "../game-entities.rkt")
;(require "../components/after-time.rkt")
(require posn)

;(displayln "LOADING ON START")

(provide (struct-out on-start))

(struct on-start (func))

(define (update-on-start g e c)
  ;(displayln (list "UPDATING ON START" e))
  (remove-component
   ((on-start-func c) g e) on-start?))

(new-component on-start?
               update-on-start)  