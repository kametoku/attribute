(in-package :attribute)

(defwidget edit-widget (object-widget)
  ((input-widgets :initform nil :accessor input-widgets)))

(defmethod reblocks/widget:get-css-classes ((widget edit-widget))
  (append '(:ui :grid) (call-next-method)))

(defmethod render-attr ((widget edit-widget) object &rest attr
                        &key label name &allow-other-keys)
  (with-html
    (:label :for name :class "four wide column" (:strong label))
    (:div :class "twelve wide field"
          (let ((w (render-field attr :object object :tag :input)))
            (when w (push w (input-widgets widget)))))))

(defmethod render-attrs ((widget edit-widget) &rest args)
  (setf (input-widgets widget) nil)
  (with-attrs (section icon object attrs) args
    (unless (functionp (car attrs))
      (section-title section icon)
      (call-next-method))))

(defun make-edit-widget (formatter object)
  (make-instance 'edit-widget :formatter formatter :object object
                              :skip-sections-p t))

(defun object-edit-page-attributes (formatter object)
  (let ((widget (make-edit-widget formatter object)))
    (render widget)
    widget))
