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
    (blue-circle-avatar)))

(require racket/rerequire)

(define meta-g
  (game
    (level-manager 
      (for/stream ([i (in-naturals)])
        (displayln "Hot Swap")

        (define temp (make-temporary-file "hotswap~a.rkt" #f here))

        (define this-s (file->string me))
        (display-to-file #:exists 'replace
          (regexp-replace #rx"(