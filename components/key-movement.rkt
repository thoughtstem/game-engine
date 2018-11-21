#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (rename-out (make-key-movement key-movement))
         key-movement?
         change-speed-by
         get-speed
         (struct-out on-no-key-movement)
         (struct-out on-key-movement)
         set-player-speed
         moving?
         player-is-moving?
         stop-movement
         )

(struct key-movement (speed mode rule?) #:transparent)

;This just puts the units we usually use into units that Chimpmunk physics understands.
(define MAGIC-SPEED-MULTIPLIER 50)

(define (make-key-movement speed #:mode [mode 'arrow-keys] #:rule [rule? (lambda (g e) #t)])
  (key-movement speed mode rule?))

(define (update-key-movement g e c)
  (define rule? (key-movement-rule? c))
  (if ((key-movement-rule? c) g e)
      (set-velocity e
                    (velocity-from-buttons  g
                                            (* MAGIC-SPEED-MULTIPLIER (key-movement-speed c))
                                            (key-movement-mode c)))
      e))

(define/contract (velocity-from-buttons game speed mode)
  (-> game? number? symbol? posn?)
  (define key-list
    (cond [(eq? mode 'arrow-keys) (list 'left  'right 'up    'down)]
          [(eq? mode 'wasd)       (list 'a     'd     'w     's)]
          [else                   (list 'left  'right 'up    'down)]))
  (define leftVel  (if (button-down? (first  key-list) game) (- speed) 0))
  (define rightVel (if (button-down? (second key-list) game)   speed  0))
  (define upVel    (if (button-down? (third  key-list) game) (- speed) 0))
  (define downVel  (if (button-down? (fourth key-list) game)   speed  0))
  (posn (+ leftVel rightVel)
        (+ upVel downVel)))

;Not clear either...  Move or simplify with better API
(define (change-speed-by n)
  (lambda (g e)
    (define increase (lambda (k)
                       (struct-copy key-movement k
                                    [speed (+ (key-movement-speed k) n)])))
    (update-entity e key-movement? increase)))

(define (get-speed e)
  (key-movement-speed (get-component e key-movement?)))

(define (get-key-mode e)
  (key-movement-mode (get-component e key-movement?)))

(define (get-current-velocity g e)
  (velocity-from-buttons g (get-speed e) (get-key-mode e)))


(new-component key-movement?
               update-key-movement)


(struct on-no-key-movement (f))

(define (update-on-stopped g e c)
  (define v (get-current-velocity g e))
  (if  (equal? (posn 0 0) v)
       ((on-no-key-movement-f c) g e)
       e))

(new-component on-no-key-movement?
               update-on-stopped)


(struct on-key-movement (f))

(define (update-on-moved g e c)
  (define v (get-current-velocity g e))
  (if  (equal? (posn 0 0) v)
       e
       ((on-key-movement-f c) g e)))

(new-component on-key-movement?
               update-on-moved)

(define (set-player-speed n)
  (lambda (g e)
    (update-entity e key-movement? (key-movement n))))

(define (stop-movement)
  (lambda (g e)
    (set-velocity e (posn 0 0))))

(define (moving? g e)
  (define vel (get-current-velocity g e))
  (not (equal? vel (posn 0 0))))

(define (player-is-moving? g e)
  (define vel (get-current-velocity g (get-entity "player" g)))
  (not (equal? vel (posn 0 0))))