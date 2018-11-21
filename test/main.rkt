#lang racket/base
 
(require rackunit
         "../main.rkt")


;Dumb test for now.  More later
(define e (backpack))
(check-equal? e e)
