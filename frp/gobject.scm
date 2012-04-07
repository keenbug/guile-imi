(define-module (imi frp gobject)
  #:use-module (imi frp)
  #:use-module (gnome glib)
  #:use-module (gnome gobject)
  #:use-module (gnome gobject gsignal)
  #:export (gobject-event gobject-callback gobject-event*
            msec-tick set-slot))


(define (gobject-event gobj signal-name)
  (make-event
    (lambda (frp-event)
      (connect gobj signal-name
        (lambda (widget gsignal-event)
          (event-trigger frp-event gsignal-event)
          #t)))
    (lambda (handler-id)
      (disconnect gobj handler-id))))

(define (gobject-callback gobj signal-name)
  (make-event
    (lambda (frp-event)
      (connect gobj signal-name
        (lambda (widget)
          (event-trigger frp-event #t)
          #t)))
    (lambda (handler-id)
      (disconnect gobj handler-id))))


(define (gobject-event* gobj signal-name)
  (make-event
    (lambda (frp-event)
      (connect gobj signal-name
        (lambda args
          (event-trigger frp-event args)
          #t)))
    (lambda (handler-id)
      (disconnect gobj handler-id))))



(define (msec-tick msecs)
  (make-event
    (lambda (frp-event)
      (g-timeout-add msecs
        (lambda ()
          (event-trigger frp-event #t)
          #t)))
    (lambda (source-id)
      (g-source-remove source-id))))



(define (set-slot gobj slot value-b)
  (set gobj slot (behavior-value value-b))
  (behavior-use value-b (lambda (new-v) (set gobj slot new-v))))
