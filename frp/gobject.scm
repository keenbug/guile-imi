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
      (format #t "using event ~s~%" signal-name)
      (connect gobj signal-name
        (lambda (widget gsignal-event)
          (event-trigger frp-event gsignal-event)
          #t)))
    (lambda (handler-id)
      (format #t "unusing event ~s~%" signal-name)
      (disconnect gobj handler-id))))

(define (gobject-callback gobj signal-name)
  (make-event
    (lambda (frp-event)
      (format #t "using callback ~s~%" signal-name)
      (connect gobj signal-name
        (lambda (widget)
          (event-trigger frp-event #t)
          #t)))
    (lambda (handler-id)
      (format #t "unusing callback ~s~%" signal-name)
      (disconnect gobj handler-id))))


(define (gobject-event* gobj signal-name)
  (make-event
    (lambda (frp-event)
      (format #t "using event* ~s~%" signal-name)
      (connect gobj signal-name
        (lambda args
          (event-trigger frp-event args)
          #t)))
    (lambda (handler-id)
      (format #t "unusing event* ~s~%" signal-name)
      (disconnect gobj handler-id))))



(define (msec-tick msecs)
  (make-event
    (lambda (frp-event)
      (format #t "start ~s tick~%" msecs)
      (g-timeout-add msecs
        (lambda ()
          (event-trigger frp-event #t)
          #t)))
    (lambda (source-id)
      (format #t "stop ~s tick (id ~s)~%" msecs source-id)
      (g-source-remove source-id))))



(define (set-slot gobj slot value-b)
  (set gobj slot (behavior-value value-b))
  (behavior-use value-b (lambda (new-v) (set gobj slot new-v))))
