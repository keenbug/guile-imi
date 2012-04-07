#!/usr/bin/guile -s
!#

(use-modules (gnome-2)
             (cairo)
             (srfi srfi-1)
             (srfi srfi-8)
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
    (vbox
      world-canvas
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
    (cairo-set-source-rgb cr 1 1 1)
    (cairo-rectangle cr oldx oldy 16 16)
    (cairo-fill cr)
    (gdk-cairo-set-source-pixbuf cr img x y)
    (cairo-paint cr)
    (cairo-stroke cr)))


(define (draw-world cr rob:state)
  (receive (width height) (gtk-widget-get-size-request world-canvas)
    (cairo-set-source-rgb cr 1 1 1)
    (cairo-rectangle cr 0 0 width height)
    (cairo-fill cr)
    (cairo-paint cr)
    (apply draw-robot cr rob:state)))
  


(define (get-world-cairo)
  (gdk-cairo-create (get world-canvas 'window)))




;;;
;;; Event utilities
;;;

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
                gdk:Right 'right))

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
(define *step* (max 1 (round (/ *robot:px/sec* *fps*)))) ; per tick stepsize
(define *sps*  (round (/ *robot:px/sec* *step*)))        ; recalculate steps/sec
(define *tick* (integer (/ 1000 *sps*)))                 ; steps/sec -> msec/step


(define (step+ x) (+ x *step*))
(define (step- x) (- x *step*))

(define refresh-tick (msec-tick *tick*))
(define keypress (gobject-event win 'key-press-event))
(define keyrelease (gobject-event win 'key-release-event))
(define expose (gobject-event world-canvas 'expose-event))

(define motionkeypress (event-filter motionkey? keypress))
(define motionkeyrelease (event-filter motionkey? keyrelease))

(define keyspressed
  (event-fold (lambda (ev pressed-keys)
                (let ((event-key (key-event->direction-symbol ev)))
                  (case (gdk-event:type ev)
                    ((key-press)
                     (if (not (memq event-key pressed-keys))
                         (cons event-key pressed-keys)
                         pressed-keys))
                    ((key-release)
                     (filter (lambda (key)
                               (not (eq? event-key key)))
                             pressed-keys))
                    (else pressed-keys))))
              (list)
              (event-merge motionkeypress motionkeyrelease)))

(define move-tick
  (event-mask (behavior-process pair? keyspressed)
              refresh-tick))

(define move-event
  (event-map second (event-snapshot move-tick keyspressed)))

(define start-position (cons 20 20))
(define robot-position
  (event-fold (lambda (directions pos)
                (fold (lambda (dir pos)
                        (pmatch pos
                          ((,x . ,y)
                           (case dir
                             ((up)    (cons x (step- y)))
                             ((down)  (cons x (step+ y)))
                             ((left)  (cons (step- x) y))
                             ((right) (cons (step+ x) y))
                             (else pos)))))
                      pos
                      directions))
              start-position
              move-event))

(set-slot statusbar 'label
          (behavior-process
            (lambda (moving position)
              (format #f "moving: ~s position: ~s" moving position))
            keyspressed
            robot-position))


(define robot-state
  (behavior-process (lambda (pos keyspressed)
                      (pmatch pos 
                        (((,x . ,y) (,oldx . ,oldy))
                         (cons (and (not (null? keyspressed))
                                    (car keyspressed))
                               (list x y oldx oldy)))))
                    (behavior-trace robot-position start-position)
                    keyspressed))


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
