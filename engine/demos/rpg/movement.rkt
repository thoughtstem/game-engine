#lang racket

(provide physical-movement)

(require "../../main.rkt"
         2htdp/image)


(define (opposite-signs a b)
  (cond
    [(and (negative? a) (positive? b)) #t]
    [(and (positive? a) (negative? b)) #t]
    [else #f]))

(define (thruster posn-? )
  (define p (as-posn (get-current-input)))
  (define ?-dir (posn-? p))

  (define v? (if (not (get-velocity)) 
               0
               (posn-? (get-velocity))))

  (define maxv 200)

  (cond 
    [(and (not (= 0 ?-dir)) ;Want to move
          (<= (abs v?) maxv))      ;Not going too fast..
      (* 100 (get-delta-time) ?-dir)]

    [(= 0 ?-dir) (- (* 10 v?))]
    [(opposite-signs v? ?-dir) (- (* 10 v?))]
    [else 0]))

(define (physical-movement)
  (physics-system 20 20
                  #:mass 1

                  #:forces 
                  (thunk* 
                    (posn
                      (thruster posn-x) 
                      (thruster posn-y)))))
