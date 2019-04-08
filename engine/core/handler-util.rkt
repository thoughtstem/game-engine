#lang racket

(provide log apply-handler apply-op compose-handlers for-ticks on-rule remove-self 
         g->g-to-handler 
         g->e-to-handler 
         g->c-to-handler 
         e->e-to-handler 
         e->c-to-handler 
         c->c-to-handler 
         handler-to-g->g
         handler-to-g->e 
         handler-to-g->c
         handler-to-e->e 
         handler-to-e->c 
         handler-to-c->c
         
         is-handler?)

(require "./crud.rkt"
         "./base.rkt")

(define (is-handler? h) 
  (and (procedure? h)
       (= 3 (procedure-arity h))))

;TODO: move to base
(define rule? (-> game? entity? component? boolean?))

;Lifting common function types to handlers
; Some kind of weird algebra...

  (define/contract (g->g-to-handler g->g)
     (-> (-> game? game?) game-handler?)              
     (lambda (g e c)
       (g->g g)) )

  (define/contract (g->e-to-handler g->e)
     (-> (-> game? entity?) entity-handler?)              
     (lambda (g e c)
       (g->e e)))

  (define/contract (g->c-to-handler g->c)
     (-> (-> game? component?) component-handler?)              
     (lambda (g e c)
       (g->c c)))

  (define/contract (e->e-to-handler e->e)
     (-> (-> entity? entity?) entity-handler?)              
     (lambda (g e c)
       (e->e e)))

  (define/contract (e->c-to-handler e->c)
     (-> (-> entity? component?) component-handler?)              
     (lambda (g e c)
       (e->c e)))

  (define/contract (c->c-to-handler c->c)
     (-> (-> component? component?) component-handler?)              
     (lambda (g e c)
       (c->c c)))


(define/contract (handler-to-g->g h)
     (-> game-handler? (-> game? game?))              
                 
     (lambda (g)
       (h g #f #f))) 

(define/contract (handler-to-g->e h)
     (-> entity-handler? (-> game? entity?))              
                 
     (lambda (g)
       (h g #f #f))) 

(define/contract (handler-to-g->c h)
     (-> component-handler? (-> game? component?))              
                 
     (lambda (g)
       (h g #f #f))) 



(define/contract (handler-to-e->e h)
     (-> entity-handler? (-> entity? entity?))              
                 
     (lambda (e)
       (h #f e #f))) 

(define/contract (handler-to-e->c h)
     (-> component-handler? (-> entity? component?))              
                 
     (lambda (e)
       (h #f e #f))) 

(define/contract (handler-to-c->c h)
     (-> component-handler? (-> component? component?))              
                 
     (lambda (c)
       (h #f #f c))) 


;HANDLERS


(define/contract (remove-self)
  (-> handler?)
  (lambda (g e c)
    (remove-component e c)))

(define/contract (log msg)
  (-> string? handler?)
  (lambda (g e c)
    (displayln msg) 
    'noop))

(define/contract (apply-handler h g e c)
  (-> handler? game? entity? component? operation?)

  (define op (h g e c))
  (apply-op op g e c))


(define/contract (on-rule r h)
  (-> rule? handler? handler?)

  (lambda (g e c)
    (if (r g e c)
        (h g e c)
        'noop)))


(define/contract (compose-handlers . hs)
   (->* () () #:rest (listof handler?) handler?)

   (lambda (g e c) 
     (define gec 
       (list g e c))

     ;Use the handler to get
     (for ([h hs]) 
       (define o (h g e c))
       (define diffs (diff o g e c)) 
       (set! gec (apply-diffs temp-g diffs)))
     
     (first gec)))

(define/contract (diff o g e c)
    (-> operation? game? entity? component? delayed-operation?)             
                 
    )

(define/contract (apply-diffs gec diffs)
    (-> gec? (listof delayed-operation?) gec?)             
    ;What does a list of delayed operations need to be in order for it to get crunched down into a single operation that gets applied once, but implies multiple changes to the same object.
    ;Simply one that we iterate over in the context of an accumulator?  NO!  Iteration over the results of a handler will not work.  The diffs therefore cannot be functions.

    ;a language that can be interpreted for its effects on a game.  Whose expressions can be "crunched" in a single pass into a single expression that unions all of the changes into one change that has the same effect, when interpreted as the longer interpretation of each expression in sequence would have taken.

    ;TODO: Figure out what this language looks like.  Then figure out how/when handlers being executed produces this program.

    )


(define (apply-op o g e c)
  (cond
    [(game? o) o]
    [(entity? o) (update-entity g e o)]
    [(component? o) (update-entity g e 
                                    (update-component e c o))]
    [(noop? o) g]
    [(done? o) (update-entity g e 
                                 (update-component e c (component-done c)))]
   ;As it stands, a list of ops has the property that some upstream changes will be completely masked by downstream ones -- even if they could theoretically have both happened...  Feels like we need a merge operation.  
    [(list? o) (apply-op-list o g e c)]
    [else (raise (~a "Unsupported handler return value: " o))]))

(define (apply-op-list os g e c)
  (if (empty? os)
      g
      (let* ([new-g (apply-op (first os) g e c)]
             [new-e (get-entity new-g e)]
             [new-c (get-component new-e c)])
        (apply-op-list (rest os)
                       new-g   
                       new-e
                       new-c))))



(define/contract (for-ticks n h)
  (-> number? handler? handler?)

  (define to-go 0)

  (lambda (g e c)
    (set! to-go (add1 to-go)) 

    ;TODO: Should we be looking at h and bailing early if it finishes early?  Or is the point of for-ticks that it spends that long on it no matter what...
    (if (> to-go n)
      'done
      (h g e c))))


