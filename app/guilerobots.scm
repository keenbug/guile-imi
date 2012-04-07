#!/usr/bin/guile -s
!#

(use-modules (gnome-2)
             (cairo)
             (srfi srfi-1)
             (srfi srfi-8)
             (srfi srfi-42)
             (ice-9 ftw)
             (system base pmatch))
(use-modules (gnome gobject)
             (gnome gobject gsignal)
             (gnome gtk)
             (gnome gtk gdk-event))

;;; I assume that this file is in the imi libs,
;;; otherwise they need to be added to the load
;;; path by hand
(eval-when (compile eval)
  (add-to-load-path
    (string-append
      (dirname (car (command-line)))
      "/../../")))


;;; now i should be able to load the imi libs
(use-modules (imi frp)
             (imi frp gobject)
             (imi gnome gtk simple))




;;;
;;; Settings
;;;


(define *fps*          60) ;frames/sec

(define *robot:speed*   2) ;fields/sec - field: 16x16 px rectangle
(define *robot:px/sec*
  (* *robot:speed* 16)) ;px/sec


(define *bin-dir* (dirname (car (command-line))))
(define *data-dir* (in-vicinity *bin-dir* "guilerobots/"))
(define *pictdir* (in-vicinity *data-dir* "xpm/"))





;;;
;;; The Interface
;;;

(define world-canvas (gtk-drawing-area-new))
(define statusbar (gtk-label-new ""))

(define win
  (container (gtk-window-new)
    (vbox #f #f
      #:fill world-canvas
      statusbar)))


(connect win 'destroy
         (lambda args
           (gtk-main-quit)
           #f))

(gtk-widget-show-all win)





;;;
;;; Some Utilities
;;;

(define (simple-assoc key value . rest)
  (if (null? rest)
      (list (cons key value))
      (cons (cons key value)
            (apply simple-assoc rest))))


(define (integer x)
  (inexact->exact (round x)))


(define (position-bounded? pos)
  (and (zero? (modulo (car pos) 16))
       (zero? (modulo (cdr pos) 16))))




;;;
;;; The Robot drawing stuff
;;;

(define (load-robot type)
  (gdk-pixbuf-new-from-file
    (in-vicinity *pictdir*
                 (if (not type)
                     "robot.xpm"
                     (format #f "robot_~a.xpm" type)))))

(define img:robot (load-robot #f))
(define img:robot-north (load-robot 'north))
(define img:robot-east  (load-robot 'east))
(define img:robot-south (load-robot 'south))
(define img:robot-west  (load-robot 'west))

(define (get-robot-picture movement)
  (case movement
    ((up)    img:robot-north)
    ((right) img:robot-east)
    ((down)  img:robot-south)
    ((left)  img:robot-west)
    (else    img:robot)))




(define (draw-robot cr movement x y oldx oldy)
  (let ((img (get-robot-picture movement)))
    (redraw-box-world cr oldx oldy)
    (gdk-cairo-set-source-pixbuf cr img x y)
    (cairo-paint cr)))



(define (redraw-box-world cr x y)
  (cairo-set-source-rgb cr 1 1 1)
  (cairo-rectangle cr x y 16 16)
  (cairo-fill cr)

  (cairo-set-source-rgb cr .7 .7 .7)
  (cairo-set-line-cap cr 'square)
  (cairo-set-line-width cr 1)
  (if (zero? (modulo x 16))
      (do-ec (:range x x (+ 32 x) 16)
             (draw-line cr x y x (+ 16 y)))
      (let ((line-x (+ x (- 16 (modulo x 16)))))
        (draw-line cr line-x y line-x (+ 16 y))))
  (if (zero? (modulo y 16))
      (do-ec (:range y y (+ 32 y) 16)
             (draw-line cr x y (+ 16 x) y))
      (let ((line-y (+ y (- 16 (modulo y 16)))))
        (draw-line cr x line-y (+ 16 x) line-y))))


(define (draw-world-grid cr width height)
  (cairo-set-source-rgb cr 0.7 0.7 0.7)
  (cairo-set-line-cap cr 'butt)
  (cairo-set-line-width cr 1)
  (do-ec (:range x 0 width 16)
         (draw-line cr x 0 x height))
  (do-ec (:range y 0 height 16)
         (draw-line cr 0 y width y)))



(define (draw-world cr rob:state)
  (receive (width height) (gdk-drawable-get-size (get world-canvas 'window))
    (cairo-set-source-rgb cr 1 1 1)
    (cairo-rectangle cr 0 0 width height)
    (cairo-fill cr)
    (cairo-paint cr)
    (draw-world-grid cr width height)
    (apply draw-robot cr rob:state)))



;;; automatically adds an offset when drawing
;;; so the lines are on the pixels and not 
;;; between (important for 1px width lines)
(define (draw-line cr x0 y0 x1 y1)
  (cairo-move-to cr (+ .5 x0) (+ .5 y0))
  (cairo-line-to cr (+ .5 x1) (+ .5 y1))
  (cairo-stroke cr))


(define (get-world-cairo)
  (gdk-cairo-create (get world-canvas 'window)))




;;;
;;; Event utilities
;;;

;;; keeps track of the last N states of b,
;;; where N is the length of inits and the
;;; initial trace is inits. The trace is a
;;; list of the last states.
(define (behavior-trace b . inits)
  (let ((n (length inits)))
    (behavior-integrate
      (lambda (new-v trace)
        (cons new-v (list-head trace n)))
      inits
      b)))




;;;
;;; Event stuff
;;;

(gtk-widget-add-events win (list 'key-press-mask 'key-release-mask))

(define *motion-keys*
  (simple-assoc gdk:Up    'up
                gdk:Down  'down
                gdk:Left  'left
                gdk:Right 'right

                gdk:w 'up
                gdk:a 'left
                gdk:s 'down
                gdk:d 'right

                gdk:h 'left
                gdk:j 'down
                gdk:k 'up
                gdk:l 'right
                ))

(define (key-event->direction-symbol ev)
  (assq-ref *motion-keys*
            (gdk-event-key:keyval ev)))

(define (motionkey? ev)
  (assq (gdk-event-key:keyval ev)
        *motion-keys*))


;;; calculate some constants
;;;  they all should be integers, first priority to be
;;;  nearest at the original is the speed, so calculate
;;;  and round steps at optimal fps and then calculate
;;;  fps new, now tick, so in the end speed will almost
;;;  be achieved
(define *step* (max 1 (floor (/ *robot:px/sec* *fps*)))) ; per tick stepsize
(define *sps*  (/ *robot:px/sec* *step*))                ; recalculate steps/sec
(define *tick* (integer (/ 1000 *sps*)))                 ; steps/sec -> msec/step


(define (step+ x) (+ x *step*))
(define (step- x) (- x *step*))

(define refresh-tick (msec-tick *tick*))
(define keypress (gobject-event win 'key-press-event))
(define keyrelease (gobject-event win 'key-release-event))
(define expose (gobject-event world-canvas 'expose-event))

(define motionkeypress (event-filter motionkey? keypress))
(define motionkeyrelease (event-filter motionkey? keyrelease))

(define keypressed
  (event-fold (lambda (ev pressed-key)
                (let ((event-key (key-event->direction-symbol ev)))
                  (case (gdk-event:type ev)
                    ((key-press) event-key)
                    ((key-release)
                     (if (eq? event-key pressed-key)
                         #f
                         pressed-key))
                    (else pressed-key))))
              #f
              (event-merge motionkeypress motionkeyrelease)))

(define on-bounds-event (make-event))
(define on-bounds (event-hold on-bounds-event #t))

(define current-move
  (behavior-integrate
    (lambda (bounds/key current-move)
      (pmatch bounds/key
        ((,on-bounds? ,pressed-key)
         (if on-bounds?
             pressed-key
             current-move))))
    #f
    (behavior-process list on-bounds keypressed)))

(define move-tick
  (event-mask current-move refresh-tick))

(define move-event
  (event-map second (event-snapshot move-tick current-move)))

(define start-position (cons 0 0))
(define robot-position
  (event-fold (lambda (direction pos)
                (pmatch pos
                  ((,x . ,y)
                   (case direction
                     ((up)    (cons x (step- y)))
                     ((down)  (cons x (step+ y)))
                     ((left)  (cons (step- x) y))
                     ((right) (cons (step+ x) y))
                     (else pos)))))
              start-position
              move-event))

(set-slot statusbar 'label
          (behavior-process
            (lambda (pressed moving position)
              (format #f "pressed: ~s moving: ~s position: ~s" pressed moving position))
            keypressed
            current-move
            robot-position))


(define robot-position-trace
  (behavior-trace robot-position start-position))

(define robot-state
  (behavior-process (lambda (pos direction)
                      (pmatch pos 
                        (((,x . ,y) (,oldx . ,oldy))
                         (cons direction
                               (list x y oldx oldy)))))
                    robot-position-trace
                    current-move))

(behavior-use robot-position-trace
              (lambda (positions)
                (let ((bounded (map position-bounded? positions)))
                  (when (not (eq? (first bounded)
                                  (second bounded)))
                    (event-trigger on-bounds-event (first bounded))))))


(behavior-use robot-state
              (lambda (state)
                (apply draw-robot (get-world-cairo) state)))


(define redraw-event (event-map second (event-snapshot expose robot-state)))

(event-use redraw-event
           (lambda (robot:state)
             (draw-world (get-world-cairo) robot:state)))


;;;
;;; Start the interface! :-)
;;;

(gtk-main)
