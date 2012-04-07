(define-module (imi gnome gtk simple)
  #:use-module (oop goops)
  #:use-module (gnome gtk)
  #:use-module (srfi srfi-1)
  #:export (container container*
            hbox hbox* vbox vbox*
            label label*
            widget widget*
            table
            ))


(define (container cont . widgets)
  (container* cont widgets))

(define (container* cont widgets)
  (for-each (lambda (widget)
              (gtk-container-add cont widget))
            widgets)
  cont)


(define (widget sth)
  (cond
    ((string? sth)
     (gtk-label-new sth))
    ((list? sth)
     (hbox* (map widget sth)))
    ((is-a? sth <gtk-widget>) sth)
    (else
     (error "cannot convert object to gtk widget: ~s" sth))))

(define (widget* . sth)
  (widget sth))




(define (box* box std-exp std-fill widgets)
  (if (null? widgets)
      box
      (case (car widgets)
        ((#:expand)
         (if (null? (cdr widgets))
             (error "box*: missing argument to #:expand")
             (begin (gtk-box-pack-start box (widget (cadr widgets)) #t #f 0)
                    (box* box std-exp std-fill (cddr widgets)))))
        ((#:fill)
         (if (null? (cdr widgets))
             (error "box*: missing argument to #:fill")
             (begin (gtk-box-pack-start box (widget (cadr widgets)) #t #t 0)
                    (box* box std-exp std-fill (cddr widgets)))))
        ((#:noexpand)
         (if (null? (cdr widgets))
             (error "box*: missing argument to #:noexpand")
             (begin (gtk-box-pack-start box (widget (cadr widgets)) #f #f 0)
                    (box* box std-exp std-fill (cddr widgets)))))
        ((#:nofill)
         (if (null? (cdr widgets))
             (error "box*: missing argument to #:nofill")
             (begin (gtk-box-pack-start box (widget (cadr widgets)) #t #f 0)
                    (box* box std-exp std-fill (cddr widgets)))))
        (else
         (gtk-box-pack-start box (widget (car widgets)) std-exp std-fill 0)
         (box* box std-exp std-fill (cdr widgets))))))


(define (vbox stdexp stdfill . widgets) (vbox* stdexp stdfill widgets))
(define (vbox* stdexp stdfill widgets)
  (box* (gtk-vbox-new) stdexp stdfill widgets))


(define (hbox stdexp stdfill . widgets) (hbox* stdexp stdfill widgets))
(define (hbox* stdexp stdfill widgets)
  (box* (gtk-hbox-new) stdexp stdfill widgets))

(define (label text . widgets)
  (label* text widgets))

(define (label* text widgets)
  (hbox* #t #t (cons* #:noexpand (gtk-label-new text)
                      widgets)))




(define (table homogeneous? . rows)
  (let ((row-size (length rows))
        (column-size (fold (lambda (row cols)
                             (lcm cols (length row)))
                           1
                           rows)))
    (let ((table-widget (gtk-table-new row-size column-size homogeneous?)))
      (let rows ((rown 0) (row rows))
        (unless (null? row)
          (let* ((row-size (length (car row)))
                 (column-width (/ column-size row-size)))
            (let columns ((columnn 0) (column (car row)))
              (if (null? column)
                  (rows (1+ rown) (cdr row))
                  (begin
                    (gtk-table-attach-defaults table-widget
                                               (widget (car column))
                                               (* column-width columnn)
                                               (* column-width (1+ columnn))
                                               rown
                                               (1+ rown))
                    (columns (1+ columnn) (cdr column))))))))
      table-widget)))
