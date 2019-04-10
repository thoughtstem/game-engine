#lang racket

#;
(provide component-diffs)

#;
(require "./base.rkt"
         "./crud-lang.rkt"
         )

;Do we need this?

#;
(define/contract (component-diffs c1 c2)
   (-> component? component? )
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

#;
(test-case "Component diff: 0 differing fields"
  (define h (health 5))

  (check-equal? 
    (length (component-diffs h h))
    0))


#;
(test-case "Component diff: 1 differing field"
  (define h (health 5))
  (define h2 (update:health/amount h add1))

  (check-equal? 
    (length (component-diffs h h2))
    1))


#;
(test-case "Component diff: 2 differing fields"
  (define-component thrusters (left right))
  (define t (thrusters #f #f))
  (define t2 (update:thrusters/left  t #t))
  (define t3 (update:thrusters/right t2 #t))

  (check-equal? 
    (length (component-diffs t t3))
    2))
