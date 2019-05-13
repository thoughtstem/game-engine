#lang racket

(require "../main.rkt"
         "./util.rkt"
         2htdp/image)

(require racket/runtime-path)

(define-runtime-path here ".")

(define me (build-path here "hotswap.rkt"))

(define prev-modified #f)

(define (file-modified? me)
  (define modified 
    (and
      (file-exists? me)
      (file-or-directory-modify-seconds me)))  

  (and modified prev-modified
       (> modified prev-modified))
  )

(provide g)
(define g 
  (game 
    input-manager
    (red-square-enemy)
    (red-square-enemy)
    (red-square-enemy)
    (red-square-enemy)

     (blue-circle-avatar (posn 180 200))
     (blue-circle-avatar (posn 200 200))
     (blue-circle-avatar (posn 220 200))

     #;
    (update-component
     position? 
     (position (posn 200 200)
               #;
               (posn-add (posn -0.1 -0.1)
                         (get-position))))))



(define (run)
(define meta-g
  (game
    (level-manager 
      (for/stream ([i (in-naturals)])
        (define temp (make-temporary-file "hotswap~a.rkt" #f here))

        (define this-s (file->string me))
        (display-to-file #:exists 'replace
          this-s
          temp)

        (set! prev-modified (file-or-directory-modify-seconds me))     

        (define ret (dynamic-require temp 'g))

        (pretty-print-game ret)

        ret)
      (thunk* (file-modified? me)))))
  (play! meta-g))










