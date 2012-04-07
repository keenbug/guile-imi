(define-module (imi gnome gtk simple)
  #:use-module (oop goops)
  #:use-module (gnome gtk)
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


(define (vbox . widgets) (vbox* widgets))
(define (vbox* widgets)
  (container* (gtk-vbox-new) widgets))


(define (hbox . widgets) (hbox* widgets))
(define (hbox* widgets)
  (container* (gtk-hbox-new) widgets))

(define (label text . widgets)
  (label* text widgets))

(define (label* text widgets)
  (hbox* (cons (gtk-label-new text)
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
