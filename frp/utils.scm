(define-module (imi frp utils)
  #:use-module (imi frp)
  #:export (current-time-b))

(define current-time-b
  (make-behavior (lambda () (current-time))))
