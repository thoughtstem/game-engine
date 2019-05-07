#lang racket 

 ;To try out the slow version, use this.
; Not sure it's slow, actually...

(begin
  (provide 
    (all-from-out posn) 
    posn-add*
    )

  (require posn))

(define (posn-add* p1 p2)
    (posn (+ (posn-x p1)
             (posn-x p2))
          (+ (posn-y p1)
             (posn-y p2))))
