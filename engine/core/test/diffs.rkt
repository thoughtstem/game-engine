#lang racket

(require rackunit "../main.rkt" "./util.rkt")


(test-case "Component diff: 0 differing fields"
  (define h (health 5))

  (check-equal? 
    (length (component-diffs h h))
    0))

(test-case "Component diff: 1 differing field"
  (define h (health 5))
  (define h2 (update:health/amount h add1))

  (check-equal? 
    (length (component-diffs h h2))
    1))

(test-case "Component diff: 2 differing fields"
  (define-component thrusters (left right))
  (define t (thrusters #f #f))
  (define t2 (update:thrusters/left  t #t))
  (define t3 (update:thrusters/right t2 #t))

  (check-equal? 
    (length (component-diffs t t3))
    2))


