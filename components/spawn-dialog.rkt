#lang racket

(require "../game-entities.rkt")
;(require "../components/after-time.rkt")
(require "./direction.rkt")
(require "./rotation-style.rkt")
(require posn)

;(displayln "LOADING ON START")

(provide spawn-dialog-spawn
         spawn-dialog-speed
         spawn-dialog-accum
         spawn-dialog-next
         (rename-out [make-spawn-dialog spawn-dialog]))

(struct spawn-dialog (spawn speed accum next))

(define (make-spawn-dialog spawn)
  (spawn-dialog spawn 1 0 #f))

(define (spawn-dialog-ready? s)
  (>= (spawn-dialog-accum s)
      (spawn-dialog-speed s)))

(define (spawn-dialog-reset s)
  (struct-copy spawn-dialog s
               [accum 0]
               [next #f]))

(define (next-spawn s)
  (define s2 (spawn-dialog-spawn s))
  (if (procedure? s2)
      (s2)
      s2))

(define (spawn-dialog-do-spawn e) 
  (lambda (s)
    (define to-spawn (next-spawn s))
    ;(define pos (get-component e posn?))
                       
    ;(define new-entity (update-entity to-spawn posn?
    ;                                  new-posn))
    
    (struct-copy spawn-dialog s
                 [next to-spawn])))

(define (spawn-dialog-inc s)
  (struct-copy spawn-dialog s
               [accum (add1 (spawn-dialog-accum s))]))

(define (update-spawn-dialog g e c)
  (define new-c (spawn-dialog-inc c))
  (if (spawn-dialog-ready? new-c)
       (update-entity e spawn-dialog? ((spawn-dialog-do-spawn e) new-c))
       (update-entity e spawn-dialog? new-c)))

(define/contract (collect-spawn-dialog es)
  (-> (listof entity?) (listof entity?))
  (define spawn-dialog (filter identity (map (λ(x) (get-component x spawn-dialog?)) es)))
  (filter identity (map spawn-dialog-next spawn-dialog)))

(define (reset-spawn-dialog es)
  #;(define maybe-spawn-dialog-reset (lambda (x) (if (spawn-dialog-ready? x)
                                              (spawn-dialog-reset x)
                                              x)))
  (map (λ(x) (if (and (get-component x spawn-dialog?)
                      (spawn-dialog-ready? (get-component x spawn-dialog?)))
                 (remove-component x spawn-dialog?)
                 x)) es))

(define (handle-spawn-dialog g)
  (define es     (game-entities g))
  (define new-es (collect-spawn-dialog es))
  (define all    (append new-es (reset-spawn-dialog es)))
  
  (struct-copy game g
               [entities all]))

(new-component spawn-dialog?
               update-spawn-dialog)

(new-game-function handle-spawn-dialog)
