#lang racket

(provide define-component)

(require "./base.rkt"
         "./crud.rkt")

(require (for-syntax racket))
(require (for-syntax racket/syntax))

(define-syntax (define-component stx)
  (syntax-case stx ()
    [(_ name (field ...))
     (with-syntax [(new-name (format-id #'name "new-~a" #'name))
                   (name-copy (format-id #'name "~a-copy" #'name)) 
                   (name? (format-id #'name "~a?" #'name))
                   (anys  (map (thunk* #'any/c) 
                               (syntax->datum #'(field ...)))) 
                   
                   ] 
        #`(begin
             (define (name? x) (and (vector? x)
                    (eq? 'name (vector-ref x 1))))

             (define/contract (name id handlers field ...)
               (-> (or/c number? #f) vector? #,@#'anys name?)
               (vector 'component 'name id handlers 
                       field ...))


             (generate-getter name field (field ...))
             ...

             (generate-setter name field (field ...))
             ...


             (generate-field-handlers name field (field ...))
             ...

             (generate-non-field-handlers name)


             (define/contract (new-name 
                                #:update (update #f) 
                                field ...)
              (->* anys 
                   [#:update (or/c (-> entity? name? entity?) 
                                   (-> name? name?) 
                                   handler? #f)]
                   name?)
               (name #f  
                     (vector (lift-to-handler update)) 
                     field ...))))]))

(define-syntax (generate-getter stx)
  (syntax-case stx ()
    [(_ name field (fields ...))
     (with-syntax 
       [(name-field (format-id #'name "~a-~a" #'name #'field) )
        (entity-name-field (format-id #'name "entity-~a-~a" #'name #'field) ) 
        (name? (format-id #'name "~a?" #'name) ) 
        (i (+ 4 (index-of 
                  (syntax->datum #'(fields ...))
                  (syntax->datum #'field))))]
       #`(begin
           (define/contract (name-field x)
             (-> component? any/c)

             (vector-ref x i))

           (define/contract (entity-name-field e)
             (-> entity? any/c)

             (name-field (get-component e name?)))
           
           ))]))

(define-syntax (generate-setter stx)
  (syntax-case stx ()
    [(_ name field (fields ...))
     (with-syntax 
       [(set-name-field (format-id #'name "set-~a-~a" #'name #'field) )
        (i (+ 4 (index-of 
                  (syntax->datum #'(fields ...))
                  (syntax->datum #'field))))]
       #`(begin
           (define/contract (set-name-field x v)
             (-> component? any/c component?)

             (define temp (vector-copy x))
             
             (vector-set! temp i v)

             temp)))]))


(define-syntax (generate-field-handlers stx)
  (syntax-case stx ()
    [(_ name field (fields ...))
     (with-syntax 
       [ (name? (format-id #'name "~a?" #'name))

         ;e.g. update-health-amount
         (update-component-field (format-id #'name "update-~a-~a" #'name #'field))
         ;e.g. update-entity-health-amount
         (update-entity-component-field (format-id #'name "update-entity-~a-~a" #'name #'field))

         ;e.g. update-entity-health
         (update-entity-component (format-id #'name "update-entity-~a" #'name))

         ;e.g. update-entity-first-health
         (update-entity-first-component (format-id #'name "update-entity-first-~a" #'name))
         ;e.g. health-amount
         (getter (format-id #'name "~a-~a" #'name #'field))
         (i (+ 4 (index-of 
                   (syntax->datum #'(fields ...))
                   (syntax->datum #'field))))]
       #`(begin

           (define (update-component-field f)
             (lambda (c)
               (define copy-c (vector-copy c))

               (vector-set! copy-c
                            i 
                            (f (getter copy-c)))

               copy-c))

           (define (update-entity-component-field f)
             (lambda (g e c)
               (update-component e c (update-component-field f))))

           ) )]))


(define-syntax (generate-non-field-handlers stx)
  (syntax-case stx ()
    [(_ name )
     (with-syntax 
       [ (name? (format-id #'name "~a?" #'name))  
         ;e.g. update-entity-health
         (update-entity-component (format-id #'name "update-entity-~a" #'name))

         ;e.g. update-entity-first-health
         (update-entity-first-component (format-id #'name "update-entity-first-~a" #'name))
         ;e.g. health-amount
         (getter (format-id #'name "~a-~a" #'name #'field)) ]
       #`(begin
           ;Returns a handler that replaces the component c with c2
           (define (update-entity-component c2)
             (lambda (g e c)
               (update-component e c c2)))

           (define (update-entity-first-component c2)
             (lambda (g e c)
               (update-component e name? c2)))
           ) )]))




