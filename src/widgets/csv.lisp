(in-package :attribute)

(defwidget csv-header-widget (object-widget)
  ())

(defmethod render-attr ((widget csv-header-widget) object &rest attr
                        &key label &allow-other-keys)
  (declare (ignore attr))
  label)

(defun make-csv-header-widget (formatter)
  (make-instance 'csv-header-widget :formatter formatter))

(defun csv-header-row (row-formatter)
  (render (make-csv-header-widget formatter)))


(defwidget csv-data-widget (object-widget)
  ())

(defmethod render-attr ((widget csv-data-widget) %object &rest attr
                        &key value format (object %object) &allow-other-keys)
  (declare (ignore attr))
  (let ((value (tkutil:ensure-value value object)))
    (format-field-value value format)))

(defun csv-data-row (row-formatter object)
  (render-object row-formatter object #'object-attr-value))
