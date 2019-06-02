#lang racket

(provide hotswap no-hotswap)

(define-syntax (hotswap stx) 
  (syntax-case stx ()
    [(hotswap name exp)
     #`(begin
         (provide name)
         (define name exp)

         (require racket/runtime-path)

         (define me (build-path #,(syntax-source stx)))

         (define prev-modified #f)

         (define (file-modified? me)
           (define modified 
             (and
               (file-exists? me)
               (file-or-directory-modify-seconds me)))  

           (and modified prev-modified
                (> modified prev-modified)))

         (define (hotswap-manager)
           (define last #f)
           (level-manager 
             (for/stream ([i (in-naturals)])
                         (displayln "HOTSWAP")
                         (define temp (make-temporary-file "hotswap~a.rkt" #f (apply build-path (drop-right (explode-path me) 1))))

                         (define this-s (file->string me))
                         (display-to-file #:exists 'replace
                                          (regexp-replace #rx"hotswap " this-s "no-hotswap ") ;To make it not launch the game on dynamic requires
                                          temp)

                         (set! prev-modified (file-or-directory-modify-seconds me))     
                         (define ret 
                           (with-handlers
                             ([exn:fail? (lambda (e)
                                           (displayln "Couldn't hotswap, due to error")
                                           (displayln e)
                                           last
                                           )])
                             (dynamic-require temp 'name))   )

                         (when ret
                           (set! last ret)
                           (pretty-print-game ret))


                         ret)
             (thunk* (file-modified? me))))

         (play! (game (hotswap-manager))) 
         

         
         )]))

(define-syntax-rule (no-hotswap name exp)
  (begin
    (provide name) 
    (define name exp)))



