(define-module (imi frp)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-31)
  #:export (<event> make-event
                    event?
                    event-trigger
                    event-use
                    event-unuse
                    event-process
                    event-map
                    event-map-filter
                    event-filter
                    event-merge
                    event-snapshot
                    event-fold
                    event-hold
                    event-mask
            <behavior> make-behavior
                       behavior?
                       behavior-value
                       behavior-change
                       behavior-use
                       behavior-unuse
                       behavior-process
                       behavior-integrate
            ))

(define-record-type <event>
  (%make-event userid activation activator deactivator users)
  event?
  (userid event-userid event-userid-set!)
  (activation event-activation event-activation-set!)
  (activator event-activator)
  (deactivator event-deactivator)
  (users event-users))

(define make-event
  (case-lambda
    ((activator deactivator)
     (%make-event 0 #f activator deactivator (make-hash-table)))
    (() (make-event (lambda (e) #t) (lambda (act) #t)))))

(define (event-activated? e)
  (and (event-activation e) #t))


(define (event-activate! e)
  (event-activation-set!
    e
    ((event-activator e) e)))

(define (event-deactivate! e)
  (and ((event-deactivator e) (event-activation e))
       (event-activation-set! e #f)))



(define (event-user-count e)
  (hash-fold (lambda (key value count)
               (1+ count))
             0
             (event-users e)))

(define (event-use e user-proc)
  (unless (event-activation e)
    (event-activate! e))
  (let ((id (event-userid e)))
    (hashq-set! (event-users e)
                id
                user-proc)
    (event-userid-set! e (1+ id))
    id))

(define (event-unuse e userid)
  (hashq-remove! (event-users e) userid)
  (if (zero? (event-user-count e))
      (event-deactivate! e)))


(define (event-trigger e v)
  (hash-for-each (lambda (userid user-proc)
                   (and user-proc
                        (user-proc v)))
                 (event-users e)))


(define (event-process e user-proc)
  (define (activate new-event)
    (event-use e (user-proc new-event)))
  (define (deactivate userid)
    (event-unuse e userid)
    #t)
  (make-event activate deactivate))


(define (event-map proc e)
  (event-process e
    (lambda (new-event)
      (lambda (v)
        (event-trigger new-event (proc v))))))

(define (event-map-filter proc e)
  (event-process e
    (lambda (new-event)
      (lambda (v)
        (and=> (proc v)
               (lambda (new-v)
                 (event-trigger new-event new-v)))))))

(define (event-filter pred e)
  (event-process e
    (lambda (new-event)
      (lambda (v)
        (and (pred v)
             (event-trigger new-event v))))))


(define (event-merge . events)
  (define (activate new-event)
    (map (lambda (e)
           (event-use e
             (lambda (ev)
               (event-trigger new-event ev))))
         events))
  (define (deactivate activations)
    (for-each (lambda (e act)
                (event-unuse e act))
              events
              activations)
    #t)
  (make-event activate deactivate))





(define-record-type <behavior>
  (%make-behavior value-proc event)
  behavior?
  (value-proc behavior-value-proc)
  (event behavior-event))




(define (event-snapshot event . behaviors)
  (define (activate new-event)
    (cons (event-use event
            (lambda (ev)
              (event-trigger new-event
                             (cons ev (map behavior-value behaviors)))))
          (map (lambda (b) (behavior-use b #f)) behaviors)))
  (define (deactivate activations)
    (event-unuse event (car activations))
    (for-each (lambda (b b-userid)
                (behavior-unuse b b-userid))
              behaviors
              (cdr activations))
    #t)
  (make-event activate deactivate))



(define (event-fold proc init event)
  (let ((bvalue init))
    (rec new-behavior
      (%make-behavior (lambda () bvalue)
                      (event-process event
                        (lambda (change-event)
                          (lambda (ev)
                            (let* ((old-v bvalue)
                                   (new-v (proc ev old-v)))
                              (unless (eqv? old-v new-v)
                                (set! bvalue new-v)
                                (behavior-changed new-behavior))))))))))

(define (event-hold event init)
  (let ((bvalue init))
    (rec new-behavior
      (%make-behavior (lambda () bvalue)
                      (event-process event
                        (lambda (change-event)
                          (lambda (ev)
                            (set! bvalue ev)
                            (behavior-changed new-behavior))))))))


(define (event-mask maskb event)
  (let ((ev-act #f))
    (define (activate new-event)
      (behavior-use maskb
        (lambda (act?)
          (if act?
              (and (not ev-act)
                   (set! ev-act (event-use event
                                  (lambda (ev)
                                    (event-trigger new-event ev)))))
              (and ev-act
                   (begin (event-unuse event ev-act)
                          (set! ev-act #f)))))))
    (define (deactivate act)
      (behavior-unuse maskb act)
      (and ev-act (event-unuse event ev-act))
      #t)
    (make-event activate deactivate)))



(define make-behavior
  (case-lambda
    ((value-proc activator deactivator)
     (%make-behavior value-proc (make-event activator deactivator)))
    ((value-proc)
     (%make-behavior value-proc (make-event)))))


(define (behavior-value b)
  ((behavior-value-proc b)))

(define (behavior-changed b)
  (event-trigger (behavior-event b) (behavior-value b)))


(define (behavior-use b user-proc)
  (event-use (behavior-event b) user-proc))

(define (behavior-unuse b user-id)
  (event-unuse (behavior-event b) user-id))


(define (behavior-process proc . behaviors)
  (rec new-behavior
    (make-behavior (lambda () (apply proc (map behavior-value behaviors)))
                   (lambda (change-event)
                     (map (lambda (b)
                            (behavior-use b
                              (lambda (new-v)
                                (behavior-changed new-behavior))))
                          behaviors))
                   (lambda (user-ids)
                     (for-each (lambda (b user-id)
                                 (behavior-unuse b user-id))
                               behaviors
                               user-ids)
                     #t))))

(define (behavior-integrate proc init b)
  (let ((seed (proc (behavior-value b) init)))
    (rec new-behavior
      (make-behavior (lambda () seed)
                     (lambda (change-event)
                       (behavior-use b
                         (lambda (new-v)
                           (set! seed (proc new-v seed))
                           (behavior-changed new-behavior))))
                     (lambda (user-id)
                       (behavior-unuse b user-id)
                       #t)))))
