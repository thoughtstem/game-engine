#lang racket

(provide component-diffs)

(require "./base.rkt"
         "./crud-lang.rkt"
         )

;Do we need this?

(define/contract (component-diffs c1 c2)
   (-> component? component? c-diff?)
   ;They really should have the same ids and be of the same type...
   ;  TODO: Beef up the contract...

   (filter identity
           (for/list
             ([i1 (in-naturals)]
              [v1 (component->list c1)]
              [v2 (component->list c2)])

             (if (not (eq? v1 v2))
               (update-f c1 i1 v2)
               #f))))


