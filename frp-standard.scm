(define-module (imi frp-standard)
  #:use-module (imi frp)
  #:export (current-time-b))

(define current-time-b
  (make-behavior (lambda () (current-time))))
