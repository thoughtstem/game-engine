#lang racket

(require "../game-entities.rkt")

(require (only-in rsound
                  default-sample-rate
                  resample-to-rate
                  rs-read
                  make-pstream
                  pstream-play
                  stop))

(provide (except-out (struct-out sound-stream) sound-stream)
         (rename-out (make-sound-stream sound-stream))
         make-sound
         set-sound-stream
         get-sound-stream
         play-sound
         play-sound-from
         stop-all-sounds
         stop-sound-streams)

(displayln "=== CLEARING SOUND THREADS ===")
(stop) ;shutting down any loose sound threads at least until a try-catch is made.

(default-sample-rate 48000)

(define (make-sound string-path)
  ;'()
  (resample-to-rate 48000 (rs-read (string->path string-path))))

(component sound-stream (ps))

(define (make-sound-stream)
  (new-sound-stream
                 ;'()
                (make-pstream)
                ))

(define (update-sound-stream g e c) e)

(define (set-sound-stream ps)
 (lambda (g e)
     (update-entity e sound-stream? (new-sound-stream ps))))

(define (get-sound-stream e)
  (sound-stream-ps (get-component e sound-stream?)))

(define (play-sound rs)
  (with-handlers ([exn:fail? (thunk* (displayln "Error while playing sound"))])
                 (lambda (g e)
                   (if (and rs
                            (get-component e sound-stream?))
                     (begin
                       (pstream-play (get-sound-stream e) rs)
                       e)
                     (begin
                       ;(displayln "WARNING: Missing sound-stream component. Sound will not play.")
                       e)
                     ))))

(define (play-sound-from entity-name rs)
  (lambda (g e)
    (define source-e (get-entity entity-name g))
    ((play-sound rs) g source-e)
    e))

(define (stop-all-sounds)
  (lambda (g e)
    (stop)
    e))

(define (stop-sound-streams)
  ;'()
  (stop))

(new-component sound-stream?
               update-sound-stream) 
