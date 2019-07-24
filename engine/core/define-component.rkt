#lang racket

(provide define-component
         lift
         ^
         CURRENT-COMPONENT)

(require "./base.rkt"
         "./crud.rkt")

(require (for-syntax racket))
(require (for-syntax racket/syntax))

(define CURRENT-COMPONENT (make-parameter #f))

(define-syntax (define-component stx)
  (syntax-case stx ()
    [(_ COMPONENT KIND?)
     (with-syntax [(new-COMPONENT (format-id #'COMPONENT "new-~a" #'COMPONENT))
                   (COMPONENT? (format-id #'COMPONENT "~a?" #'COMPONENT)) 
                   (COMPONENT=? (format-id #'COMPONENT "~a=?" #'COMPONENT)) 
                   (get-COMPONENT (format-id #'COMPONENT "get-~a" #'COMPONENT)) 
                   (set-COMPONENT (format-id #'COMPONENT "set-~a" #'COMPONENT)) 
                   ]
       (quasisyntax/loc stx (begin
                              (define (COMPONENT? x) 
                                (and (vector? x)
                                     (eq? 'COMPONENT (vector-ref x 1))))

                              (define 
                                (new-COMPONENT id handler handler-code FIELD)
                                (vector 'component 'COMPONENT id handler handler-code FIELD))

                              (define (COMPONENT=? v (e #f))
                                (if e
                                  (and
                                    (has-component e 'COMPONENT)
                                    (equal? v (get-COMPONENT e)))    
                                  (lambda (e)
                                    (and
                                      (has-component e 'COMPONENT)
                                      (COMPONENT=? v e)))))

                              (define (get-COMPONENT (c #f) (fail (void)))

                                (define real-c 
                                  (if c
                                      (if (entity? c)
                                          (get-component c 'COMPONENT )
                                          c)
                                    (get-component (CURRENT-ENTITY) 
                                                   'COMPONENT)))

                                (if (not real-c)
                                  (if (void? fail)
                                    (error (~a "No component " 'COMPONENT " found"))
                                    fail)
                                  (vector-ref
                                    real-c 
                                    5))) 

                              (define (set-COMPONENT e v)
                                (define to-update
                                  (if (entity? e)
                                    (get-component e COMPONENT?)
                                    e))

                                (vector-set!
                                  to-update 
                                  5
                                  v)

                                
                                e)

                              (define-syntax COMPONENT 
                                (syntax-rules ()
                                  [(COMPONENT FIELD)
                                   (new-COMPONENT (next-id)
                                                  #f
                                                  'noop
                                                  FIELD
                                                  )]
                                  [(COMPONENT FIELD update)
                                   (new-COMPONENT (next-id)  
                                                  (lambda (c)
                                                    (define new-c
                                                      (if (mutable-state)
                                                        c
                                                        (vector-copy c)))

                                                    (parameterize ([CURRENT-COMPONENT new-c])
                                                      (vector-set! new-c 5 update)) 

                                                    new-c)
                                                  'update
                                                  FIELD)
                                   ]
                                  )  

                                ))))]))



(define (lift f)
  (f (get-value (CURRENT-COMPONENT))))

(define ^ lift)
