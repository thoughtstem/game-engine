#lang racket

#;(
(require "../game-entities.rkt")
(require "./counter.rkt")
(require "./backdrop.rkt")

(provide (struct-out active-on-bg)
         active-on-random)

#;(provide (rename-out (make-active-on-bg active-on-bg))
         active-on-bg?)

;(struct active-on-bg (bg-list))

;(define (make-active-on-bg . bg-list)
;  (active-on-bg bg-list))

(define (update-active-on-bg g e c)
  (define num-or-list (active-on-bg-bg-list c))
  (define current-bg-index (if (get-component (get-entity "bg" g) backdrop?)
                               (get-current-tile (get-entity "bg" g))
                               (get-counter (get-entity "bg" g))))
  (define bg-list (if (list? num-or-list)
                      num-or-list
                      (list num-or-list)))
  (if (member current-bg-index bg-list)
      (remove-component e disabled?)
      (add-component (remove-component e disabled?)
                     (disabled))))

(new-component active-on-bg?
               update-active-on-bg)

(define (active-on-random min max)
  (lambda (g e)
     (update-entity e active-on-bg? (active-on-bg (random min (add1 max))))))

)