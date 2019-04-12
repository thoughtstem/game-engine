#lang racket

(provide debug-tick
         debug-ticks)

(require "../core/main.rkt")

(define (debug-tick g)
  (define ret (tick g)) 

  (pretty-print-game ret)

  ret)

(define (debug-ticks n g)
  (if (= 0 n)
      g
      (debug-ticks (sub1 n) (debug-tick g))))
