#lang racket

(require rackunit 
         "../main.rkt"
         "./util.rkt")

(require threading)


(define-component conway (x y alive?))          

(define (die c)  (set-conway-alive? c #f))
(define (live c) (set-conway-alive? c #t))

(define/contract (entity-xy=? x y)
                 (-> number? number?
                     (-> entity? boolean?))

                 (lambda (e)
                   (define my-x (entity-conway-x e))  
                   (define my-y (entity-conway-y e))  

                   (and 
                     (= my-x x)
                     (= my-y y))))

(define/contract (neighbor dx dy)
                 (-> number? number? (-> game? entity? (or/c entity? #f)))

                 (lambda (g e)
                   (define my-x (entity-conway-x e))
                   (define my-y (entity-conway-y e))
                   (get-entity g
                               (entity-xy=? (+ my-x dx)
                                            (+ my-y dy)))))

(define north      (neighbor  0 -1))
(define south      (neighbor  0  1))
(define east       (neighbor  1  0))
(define west       (neighbor -1  0))
(define north-west (neighbor -1 -1))
(define north-east (neighbor  1 -1))
(define south-west (neighbor -1  1))
(define south-east (neighbor  1  1))

(define (live-neighbors g e)
  (filter entity-conway-alive? 
          (filter identity
                  (list (north g e)
                        (south g e)
                        (west g e)
                        (east g e)
                        (north-east g e)
                        (north-west g e)
                        (south-east g e)
                        (south-west g e))))) 

(define (conway-update g e c)
  (define n (length (live-neighbors g e)))

  (define ret (cond 
                [(and (conway-alive? c)
                      (< n 2))
                 (die c)]
                [(and (conway-alive? c)
                      (or (= n 2) (= n 3)))
                 (live c)]
                [(and (conway-alive? c)
                      (> n 3))
                 (die c)]
                [(and (not (conway-alive? c))
                      (= n 3))
                 (live c)]
                [else c]))

  ret)

(define (conway-game size)
  (define es 
    (for*/list ([y (range size)]
                [x (range size)])
      (entity (conway x y #f #:update conway-update))))

  (apply game es))

(define (conway-game-set g x y alive?)
  ;Could also break this into a two-step process by
  ; using the update-e op.   That op can be returned by a handler 
  (update-entity g
                 (entity-xy=? x y)

                 (lambda (e)
                   (update-component e conway?
                                      (curryr set-conway-alive? alive?)))))



(define (conway-game->symbols g)
  (define es (game-entities g))  
  (define n (sqrt (length es)))

  (define (cell->symbol b)
    (if b '* '_))

  (for*/list ([x (range n)])
    (define row (take (drop es (* x n)) n))
    (map (compose cell->symbol
                  entity-conway-alive?) row))) 

(define (print-symbols ls)
  (map displayln ls))

(define (get-entity-conway-alive? e)
  (conway-alive?
    (get-component e conway?)))

(define (all-alive g)
  (define es (game-entities g)) 
  (andmap get-entity-conway-alive? es))

(define (all-dead g)
  (define es (game-entities g)) 
  
  (not (ormap get-entity-conway-alive? es)) )




(test-case "Conway's game of life"
           (define g0 (~> (conway-game 3)
                          (conway-game-set _ 0 0 #t)
                          (conway-game-set _ 1 0 #t)
                          (conway-game-set _ 2 0 #t)
                          (conway-game-set _ 0 1 #t)
                          (conway-game-set _ 1 1 #t)
                          (conway-game-set _ 2 1 #t)
                          (conway-game-set _ 0 2 #t)
                          (conway-game-set _ 1 2 #t)
                          (conway-game-set _ 2 2 #t)))

           (define gs (tick-list g0 3))

           (check-true
             (all-alive (first gs)))

           (check-true
             (all-dead (third gs))))

(test-case "Bigger Conway's game of life"
           (define g0 (~> (conway-game 5) (conway-game-set _ 1 1 #t)
                          (conway-game-set _ 2 1 #t)
                          (conway-game-set _ 3 1 #t)
                          (conway-game-set _ 1 2 #t)
                          (conway-game-set _ 2 2 #t)
                          (conway-game-set _ 3 2 #t)
                          (conway-game-set _ 1 3 #t)
                          (conway-game-set _ 2 3 #t)
                          (conway-game-set _ 3 3 #t)))

           (define gs (tick-list g0 3))


           (print-symbols (conway-game->symbols (first gs)))
           (print-symbols (conway-game->symbols (second gs)))
           (print-symbols (conway-game->symbols (third gs)))

           (check-equal?
             (conway-game->symbols (first gs))
             '((_ _ _ _ _)
               (_ * * * _)
               (_ * * * _)
               (_ * * * _)
               (_ _ _ _ _)))


           (check-equal?
             (conway-game->symbols (second gs))
             '((_ _ * _ _)
               (_ * _ * _)
               (* _ _ _ *)
               (_ * _ * _)
               (_ _ * _ _)))


           (check-equal?
             (conway-game->symbols (third gs))
             '((_ _ * _ _)
               (_ * * * _)
               (* * _ * *)
               (_ * * * _)
               (_ _ * _ _))))


