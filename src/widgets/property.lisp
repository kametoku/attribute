(in-package :attribute)

(defwidget property-widget (object-widget)
  ())

(defmethod render-attr ((widget property-widget) object &rest attr
                        &key label edit-only &allow-other-keys)
  (declare (ignore attr))
  (unless edit-only
    (with-html
      (:tr (:td :class "three wide" label)
           (call-next-method)))))

(defmethod render-attrs ((widget property-widget) &rest args)
  (with-attrs (section icon object attrs) args
    (section-title section icon)
    (if (functionp (car attrs))
        (funcall (car attrs) object)
        (with-html
          (:table :class "ui definition table"
                  (:tbody (call-next-method)))))))

(defun make-property-widget (formatter object)
  (make-instance 'property-widget :formatter formatter :object object))

(defun object-page-attributes (formatter object)
  (render (make-property-widget formatter object)))

