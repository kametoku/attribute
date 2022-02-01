(in-package :attribute)

;;; csv-header-widget

(defwidget csv-header-widget (object-widget)
  ())

(defmethod render-attr ((widget csv-header-widget) object &key label
                        &allow-other-keys)
  label)

(defun make-csv-header-widget (formatter object-type)
  (make-instance 'csv-header-widget
                 :formatter formatter :object object-type))

(defun make-csv-header-row (formatter object-type)
  (render-object (make-csv-header-widget formatter object-type)))


;;; csv-data-widget

(defwidget csv-data-widget (object-widget)
  ())

(defmethod render-attr ((widget csv-data-widget) %object
                        &key value format (object %object) &allow-other-keys)
  (let ((value (tkutil:ensure-value value object)))
    (or (format-field-value value format) "")))

(defun make-csv-data-widget (formatter object)
  (make-instance 'csv-data-widget :formatter formatter :object object))

(defun make-csv-data-row (formatter object)
  (render-object (make-csv-data-widget formatter object)))


;;; write-csv

(defun write-csv (stream formatter objects object-type)
  (write-char #\ZERO_WIDTH_NO-BREAK_SPACE stream) ; BOM
  (let ((header-row (make-csv-header-row formatter object-type)))
    (cl-csv:write-csv-row header-row :stream stream))
  (loop for object in objects
        for row = (make-csv-data-row formatter object)
        do (cl-csv:write-csv-row row :stream stream)))
