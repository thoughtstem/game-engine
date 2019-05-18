#lang racket

(provide 
  physics-manager
  (rename-out [make-physics-system physics-system]))

(require "../../core/main.rkt" posn)

(require (prefix-in chip: racket-chipmunk))

(define-component physics-system entity?)
(define-component chipmunk any/c)

(define (make-physics-system x y w h)
  (list
    (physics-system 
      (entity 
        (chipmunk
          ;TODO: Should we use these or go to the lowlevel ffi and build up from there?
          ;Let's just put together a proof of concept with the high level
          (chip:box x y 
                    w h #:meta (next-id)))))))


(define-component physics-world any/c)

(define physics-manager
  (entity
    (physics-world (void)
                   (chip:step-chipmunk (/ 1 120.0)))))

(chip:set-postsolve!
  (λ (c1 c2)
     #;
     (displayln (~a "Postsolve "
                    (chip:chipmunk-meta c1) " "
                    (chip:chipmunk-meta c2)))
     #t))

(chip:set-presolve!
  (λ (c1 c2)
     #;
     (displayln (~a "Presolve "
                    (chip:chipmunk-meta c1) " "
                    (chip:chipmunk-meta c2)))
     #t))

(chip:set-begin!
  (λ (c1 c2)
     #;
     (displayln (~a "Begin "
                    (chip:chipmunk-meta c1) " "
                    (chip:chipmunk-meta c2)))
     #t))

(chip:set-separate! (λ (c1 c2)
                       #;
                       (displayln (~a "Separate "
                                      (chip:chipmunk-meta c1) " "
                                      (chip:chipmunk-meta c2)))
                       #t))

#;
(chip:set-velocity! b 50 0)

