#lang racket

(require rackunit 
         "../main.rkt"
         "./util.rkt")

(require threading)

(provide conway-game
         conway-game-set
         conway-x
         conway-y
         conway-alive?
         entity-conway-alive?
         conway?
         overlay
         above
         beside)


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

  (define ret 
    (update-component e c
                      (cond 
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
                        [else c])))

  ret)


;TODO: Refactor this test.  It's getting too long.

;Move the conway stuff to another folder.  It's become a language in and of itself, worth testing and docing seprately.  Not part of core.

(define (pad-height g n)
  (define _n/2 (floor (/ n 2)))
  (define n/2  (ceiling (/ n 2)))
  
  (define pad-row
    (map (const '_) (range (width g))))

  (define padding-top
    (map (const pad-row) (range _n/2)))

  (define padding-bottom
    (map (const pad-row) (range n/2)))

  (append padding-top
          g
          padding-bottom))

(define (pad-width g n)
  (define _n/2 (floor (/ n 2)))
  (define n/2  (ceiling (/ n 2)))

  (define padding-left
    (map (const '_) (range _n/2))) 

  (define padding-right
    (map (const '_) (range n/2))) 

  (define (pad-row r)
    (append padding-left
            r
            padding-right))

  (map pad-row g))

(define (height g)
  (length g))

(define (width g)
  (length (first g)))

(define (expand-height-to g1 g2)
  (define diff
    (- (height g2) (height g1)))

  (if (positive? diff) 
    (pad-height g1 diff)
    g1) )

(define (expand-width-to g1 g2)
  (define diff
    (- (width g2) (width g1)))

  (if (positive? diff) 
    (pad-width g1 diff)
    g1))

(define (expand-to g1 g2)
  (define g1-ew  (expand-width-to g1 g2)) 
  (define g1-ewh (expand-height-to g1-ew g2)) 
  g1-ewh)

(define (overlay-symbol s1 s2)
  (if (eq? s1 '_)
    s2
    s1))

(define (overlay-row r1 r2)
  (map overlay-symbol r1 r2))

(define (overlay g1 g2)
  (define s1 (conway-game->symbols g1)) 
  (define s2 (conway-game->symbols g2)) 
  
  (define ps1 (expand-to s1 s2))
  (define ps2 (expand-to s2 s1))

  (grid->conway
      (map overlay-row ps1 ps2)))

(define (above g1 g2)
  (define s1 (conway-game->symbols g1)) 
  (define s2 (conway-game->symbols g2)) 
  
  (define ps1 (expand-width-to s1 s2))

  (grid->conway
      (append ps1 s2)))

(define (beside g1 g2)
  (define s1 (conway-game->symbols g1)) 
  (define s2 (conway-game->symbols g2)) 
  
  (define ps1 (expand-height-to s1 s2))

  (grid->conway
      (map append ps1 s2)))


(define (size->conway size)
  (define es 
    (for*/list ([y (range size)]
                [x (range size)])
      (entity (conway x y #f #:update conway-update))))

  (apply game es))

(define (grid->conway grid)
  (define es 
    (flatten
      (for/list ([row grid]
                 [y (in-naturals)])

        (for/list ([col row]
                   [x (in-naturals)])


          (define alive? (eq? col '*))

          (entity (conway x y alive? #:update conway-update))))))

  (apply game es))

(define (conway-game size-or-grid)
  (if (number? size-or-grid)
    (size->conway size-or-grid) 
    (grid->conway size-or-grid)))

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
  (define n (add1 (apply max (map entity-conway-y (game-entities g)))))

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
           (define g0 (~> (conway-game 5) 
                          (conway-game-set _ 1 1 #t)
                          (conway-game-set _ 2 1 #t)
                          (conway-game-set _ 3 1 #t)
                          (conway-game-set _ 1 2 #t)
                          (conway-game-set _ 2 2 #t)
                          (conway-game-set _ 3 2 #t)
                          (conway-game-set _ 1 3 #t)
                          (conway-game-set _ 2 3 #t)
                          (conway-game-set _ 3 3 #t)))

           (define gs (tick-list g0 3))

           #;
           (begin
             (print-symbols (conway-game->symbols (first gs)))
             (print-symbols (conway-game->symbols (second gs)))
             (print-symbols (conway-game->symbols (third gs))))

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


