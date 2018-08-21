#lang racket

(require "../game-entities.rkt")
(require "./counter.rkt")

(provide (struct-out active-on-bg))

#;(provide (rename-out (make-active-on-bg active-on-bg))
         active-on-bg?)

;(struct active-on-bg (bg-list))

;(define (make-active-on-bg . bg-list)
;  (active-on-bg bg-list))

(define (update-active-on-bg g e c)
  (define num-or-list (active-on-bg-bg-list c))
  (define current-bg-index (get-counter (get-entity "bg" g)))
  (define bg-list (if (list? num-or-list)
                      num-or-list
                      (list num-or-list)))
  (if (member current-bg-index bg-list)
      (remove-component e disabled?)
      (add-component (remove-component e disabled?)
                     (disabled))))

(new-component active-on-bg?
               update-active-on-bg)