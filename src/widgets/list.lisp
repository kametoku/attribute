(in-package :attribute)

(defwidget list-widget (object-widget)
  ())

(defmethod reblocks/widget:get-html-tag ((widget list-widget))
  :tr)

(defmethod render-attr ((widget list-widget) object &rest attr
                        &key edit-only insignificant
                        &allow-other-keys)
  (declare (ignore attr))
  (unless (or edit-only insignificant)
    (call-next-method)))

(defun make-list-widget (formatter object)
  (make-instance 'list-widget :formatter formatter :object object
                              :skip-sections-p t))

(defun object-list-page-attributes (formatter object)
  (render (make-list-widget formatter object)))
