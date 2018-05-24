#lang racket

(require "../game-entities.rkt")

(provide (struct-out on-rule))

;(provide (rename-out (make-on-edge on-edge)))

(struct on-rule (rule? func))

(define (update-on-rule g e c)
  (if ((on-rule-rule? c) g e)
      ((on-rule-func c) g e)
      e))

(new-component on-rule?
               update-on-rule)
