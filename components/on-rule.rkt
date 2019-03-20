#lang racket

(require "../game-entities.rkt")

(provide (except-out (struct-out on-rule) on-rule)
         (rename-out (new-on-rule on-rule))
         remove-rule)

(component on-rule (rule? func))

(define (update-on-rule g e c)
  (if ((on-rule-rule? c) g e)
      ((on-rule-func c) g e)
      e))

(define (remove-rule)
  (lambda (g e)
    (remove-component e on-rule?)))

(new-component on-rule?
               update-on-rule)
