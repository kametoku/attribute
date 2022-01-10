(in-package :attribute)

(defwidget column-widget (object-widget) ())

(defmethod render-attr ((widget column-widget) object &rest attr
                        &key label colname edit-only insignificant format
                        &allow-other-keys)
  (declare (ignore attr))
  (unless (or edit-only insignificant)
    (let ((date-column-p (member format *date-formatters*)))
      (list label colname date-column-p))))
  
(defun make-column-widget (formatter object-type)
  (make-instance 'column-widget :formatter formatter :object object-type
                                :skip-sections-p t))

(defun build-columns (formatter object-type)
  (render-object (make-column-widget formatter object-type)))
