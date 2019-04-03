#lang racket

(require "../game-entities.rkt")

(require "../engine/extensions/sound.rkt")

(provide (except-out (struct-out sound-stream) sound-stream)
         (rename-out (construct-sound-stream sound-stream))
         (rename-out ( sound make-sound))
         set-sound-stream
         get-sound-stream
         play-sound
         play-sound-from
         stop-all-sounds
         stop-sound-streams
         rsound?)


(component sound-stream (ps))

(define (update-sound-stream g e c) e)

(define (construct-sound-stream)
  (with-handlers ([exn:fail? (thunk* (displayln "Error creating sound stream"))])
    (new-sound-stream
     (make-sound-stream))))

(define (set-sound-stream ps)
 (lambda (g e)
     (update-entity e sound-stream? (new-sound-stream ps))))

(define (get-sound-stream e)
  (sound-stream-ps (get-component e sound-stream?)))

(define (play-sound rs)
  (with-handlers ([exn:fail? (thunk* (displayln "Error while playing sound"))])
    (lambda (g e)
      (if (and (sound? rs)
               (get-component e sound-stream?))
          (begin
            (play (get-sound-stream e) rs)
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
    (stop-sound-streams)
    e))

(define (stop-sound-streams)
  ;'()
  (stop-sound-streams)
  
  )

(new-component sound-stream?
               update-sound-stream) 
