#lang racket

(require game-engine
         2htdp/image
         threading )

;TODO: Finally figure out delta time.  Be able to make FPS-stable animations.


(define elf (bitmap "./images/darkelf-sheet.png"))
(define elves-up (sheet->list elf #:row 2))
(define elves-right (sheet->list elf #:row 2))
(define elves-left  (sheet->list elf #:row 1))
(define elves-down  (sheet->list elf #:row 0))

(define (hold n st)
  (stream-map
    (lambda (x)
      (if (< x n) x n))
    st))

(define (clip n st)
  (stream-map
    (curry + n)
    st))

(define (between a b st)
  (hold b (clip a st)) )


(define START 100)
(define LENGTH 100)
(define EASE 
  (lambda (x) 
    (expt x (/ 1 5)))
  #;
  sin
  #;
  sqr)

(define numbers 
  (stream-map
    (compose
      (lambda (x) 
        (define from-start (- x START))
        (define percent-complete (/ from-start LENGTH))
        (define new-percent (EASE percent-complete))

        (+ START
           (* new-percent x))))

    (between START 
             (+ START LENGTH)
             (in-naturals))
    
    ))


(define-component time-now number?)
(define-component time-then number?)
(define-component delta-time number?)

(define (stream-drop st n)
  (if (= 0 n)
    st
    (stream-drop (stream-rest st) (sub1 n))))

(define bouncing-ball
  (entity
    ;Not sure what I'm doing here...
    (time-now #f (current-inexact-milliseconds))
    (delta-time 0
                (/
                  (if (get-time-then) 
                    (- (get-time-now) (get-time-then))
                    0)
                  1000))

    ;Number stream begins at 100 and holds after 200
    (number-stream numbers
                   (stream-rest (get-number-stream))

                   ;Account for dropped frames
                   ;Rethink this.  Got tired and lost.
                   #;
                   (stream-drop 
                     (get-number-stream)
                     (if (not (get-delta-time))
                       0
                       (floor (/ (get-delta-time) 0.6)))))

    (position (posn 200 200)
              (posn 
                #;
                (stream-first (get-number-stream))      
                (+ (posn-x (get-position))
                   (* (get-delta-time)
                      10) )
                (~> (stream-first (get-number-stream))
                    (/ _ 10)  ;Frequency
                    sin
                    (* _ 10)  ;Amplitude
                    (+ _ 200) ;Shift
                    )

                ))

    (animation-system 
      #:direction-update (const (posn 1 0))
      elves-up elves-right elves-down elves-left)

    (time-then #f (current-inexact-milliseconds)))
  )

(define spinning-square
  (entity
    (position (posn 200 200))
    (sprite (register-sprite (square 20 'solid 'red)))))




(define g (game bouncing-ball spinning-square))

(play! g)
