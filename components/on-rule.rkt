#lang racket

(require "../game-entities.rkt")

(provide (struct-out on-rule)
         remove-rule)

;(provide (rename-out (make-on-edge on-edge)))

(struct on-rule (rule? func))

(define (update-on-rule g e c)
  (if ((on-rule-rule? c) g e)
      ((on-rule-func c) g e)
      e))

(define (remove-rule)
  (lambda (g e)
    (remove-component e on-rule?)))

(new-component on-rule?
               update-on-rule)
