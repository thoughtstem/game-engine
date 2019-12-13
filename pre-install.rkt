#lang racket

(provide pre-installer)

(require pkg/name pkg/lib pkg setup/setup)

(define (pre-installer path)
 (when (installed? "racket-chipmunk")
  (pkg-update-command 
   "https://github.com/thoughtstem/racket-chipmunk.git#master"
    #:deps 'search-auto #:no-setup #t)))

(define (installed? s)
 (pkg-directory s))
