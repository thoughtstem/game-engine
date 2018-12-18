#lang racket

(require "../game-entities.rkt")
;(require "../components/after-time.rkt")
(require "./direction.rkt")
(require "./rotation-style.rkt")
(require "./animated-sprite.rkt")
(require posn
         threading)

;(displayln "LOADING ON START")

(provide spawn-many-from
         spawn-once-spawn
         spawn-once-speed
         spawn-once-accum
         spawn-once-next
         was-spawned?
         spawned ;don't provide this
         (rename-out [make-spawn-once spawn-once]))

(struct spawn-once (spawn speed accum next relative?))

(struct spawned ())

(define (was-spawned? e)
  (get-component e spawned?))

(define (make-spawn-once spawn #:relative? [relative? #t])
  (spawn-once (add-component spawn
                             (spawned)) 1 0 #f relative?))

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
    (define relative? (spawn-once-relative? s #;(get-component e spawn-once?)))
    ;(displayln (~a "Spawning from: " (get-name e) ", Relative: " relative?))
    (define pos (get-component e posn?))
    (define dir (if (get-component e direction?)
                    (get-direction e)
                    #f))
    (define offset (get-component to-spawn posn?))
    (define rot-offset (unless (eq? dir #f)
                         (posn-rotate-origin-ccw (modulo (exact-round dir) 360) offset)))
    (define rs? (get-component e rotation-style?))
    (define m (if rs?
                  (rotation-style-mode rs?)
                  #f))

    
    (define facing-right?
      (if (eq? m 'left-right)
          (positive? (animated-sprite-x-scale (get-component e animated-sprite?)))
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
                       
    (define new-entity (if (and (get-component to-spawn direction?)
                                (get-component e direction?))
                           (~> to-spawn
                               (update-entity _ posn? new-posn)
                               (update-entity _ direction? (direction dir)))
                           (update-entity to-spawn posn?
                                      new-posn)))
    
    (if relative?
        (struct-copy spawn-once s
                     [next new-entity])
        (struct-copy spawn-once s
                     [next to-spawn]))))

(define (spawn-once-inc s)
  (struct-copy spawn-once s
               [accum (add1 (spawn-once-accum s))]))

(define (update-spawn-once g e c)
  (define new-c (spawn-once-inc c))
  (if (spawn-once-ready? new-c)
      (update-entity e
                     (component-is? c)
                     ((spawn-once-do-spawn e) new-c))
      e))

(define/contract (collect-spawn-once es)
  (-> (listof entity?) (listof entity?))
  (define spawn-onces (flatten (map (curryr get-components spawn-once?) es))) ;get-components?
  (filter identity (map spawn-once-next spawn-onces)))

(define (reset-spawn-once es)
  (map (λ(x)
         (define s (get-component x spawn-once?))
         (if (and s (spawn-once-ready? s))
             (remove-component x (and/c
                                  ;(component-is? s)
                                  spawn-once?
                                  spawn-once-ready?))
             x))
       es))

(define (handle-spawn-once g)
  (define es     (game-entities g))
  (define new-es (collect-spawn-once es))

  (and (not (empty? new-es))
       (displayln (~a "Spawning: " (map get-name new-es))))
  
  (define all    (append #;new-es
                         (map (curry uniqify-id g) new-es)
                         (reset-spawn-once es)))
  
  (struct-copy game g
               [entities all]))


(define (spawn-many-from source to-spawn #:relative (r #t))
  (add-components source (map (curry make-spawn-once #:relative? r) to-spawn)))


(new-component spawn-once?
               update-spawn-once)

(new-game-function handle-spawn-once)



