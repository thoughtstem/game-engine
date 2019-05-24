#lang racket

(provide do-many
         loose-component?
         component-or-system?)

(require "./game-entities.rkt"
         "./components/observe-change.rkt")

(define (do-many . funs)
  (lambda (g e)
    (foldl (lambda (next accum)
             (next g accum)) e funs)))

;TODO: Add other simple structs to this list or convert all simple structs
(define loose-component?
  (or/c component? observe-change? #f))

(define (component-or-system? c-or-list)
  ((or/c loose-component? (listof loose-component?))
   (flatten c-or-list)))

