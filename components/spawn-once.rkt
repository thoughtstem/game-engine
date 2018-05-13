#lang racket

(require "../game-entities.rkt")
;(require "../components/after-time.rkt")
(require "./direction.rkt")
(require "./rotation-style.rkt")
(require posn)

;(displayln "LOADING ON START")

(provide spawn-once-spawn
         spawn-once-speed
         spawn-once-accum
         spawn-once-next
         (rename-out [make-spawn-once spawn-once]))

(struct spawn-once (spawn speed accum next))

(define (make-spawn-once spawn)
  (spawn-once spawn 1 0 #f))

(define (spawn-once-ready? s)
  (>= (spawn-once-accum s)
      (spawn-once-speed s)))

(define (spawn-once-reset s)
  (struct-copy spawn-once s
               [accum 0]
               [next #f]))

(define (next-spawn s)
  (define s2 (spawn-once-spawn s))
  (if (procedure? s2)
      (s2)
      s2))

(define (spawn-once-do-spawn e) 
  (lambda (s)
    (define to-spawn (next-spawn s))
    (define pos (get-component e posn?))
    (define dir (if (get-component e direction?)
                    (get-direction e)
                    #f))
    (define offset (get-component to-spawn posn?))
    (define rot-offset (unless (eq? dir #f)(posn-rotate-origin-ccw dir offset)))
    (define rs? (get-component e rotation-style?))
    (define m (if rs?
                  (rotation-style-mode rs?)
                  #f))
    (define facing-right? (if (eq? m 'left-right)
                              (rotation-style-facing-right? rs?)
                              #t))
    (define new-posn (cond
                       [(and (eq? m 'left-right) (eq? facing-right? #t)) (posn (+ (posn-x pos) (posn-x offset))
                                                                               (+ (posn-y pos) (posn-y offset)))]
                       [(and (eq? m 'left-right) (eq? facing-right? #f)) (posn (- (posn-x pos) (posn-x offset))
                                                                               (+ (posn-y pos) (posn-y offset)))]
                       [(eq? m 'face-direction) (posn (+ (posn-x pos) (posn-x rot-offset))
                                                      (+ (posn-y pos) (posn-y rot-offset)))]
                       [else (posn (+ (posn-x pos) (posn-x offset))
                                   (+ (posn-y pos) (posn-y offset)))]))
                       
    (define new-entity (update-entity to-spawn posn?
                                      new-posn))
    
    (struct-copy spawn-once s
                 [next new-entity])))

(define (spawn-once-inc s)
  (struct-copy spawn-once s
               [accum (add1 (spawn-once-accum s))]))

(define (update-spawn-once g e c)
  (define new-c (spawn-once-inc c))
  (if (spawn-once-ready? new-c)
       (update-entity e spawn-once? ((spawn-once-do-spawn e) new-c))
       (update-entity e spawn-once? new-c)))

(define/contract (collect-spawn-once es)
  (-> (listof entity?) (listof entity?))
  (define spawn-once (filter identity (map (λ(x) (get-component x spawn-once?)) es)))
  (filter identity (map spawn-once-next spawn-once)))

(define (reset-spawn-once es)
  #;(define maybe-spawn-once-reset (lambda (x) (if (spawn-once-ready? x)
                                              (spawn-once-reset x)
                                              x)))
  (map (λ(x) (if (and (get-component x spawn-once?)
                      (spawn-once-ready? (get-component x spawn-once?)))
                 (remove-component x spawn-once?)
                 x)) es))

(define (handle-spawn-once g)
  (define es     (game-entities g))
  (define new-es (collect-spawn-once es))
  (define all    (append new-es (reset-spawn-once es)))
  
  (struct-copy game g
               [entities all]))

(new-component spawn-once?
               update-spawn-once)

(new-game-function handle-spawn-once)
