(in-package :attribute)

(defwidget new-widget (edit-widget)
  ())

;; (defmethod reblocks/widget:get-css-classes ((widget new-widget))
;;   (append '(:ui :grid) (call-next-method)))

(defmethod render-attr ((widget new-widget) %object &rest attr
                        &key name hide-in-new (object %object) &allow-other-keys)
  (when (and name (not hide-in-new))
    (setf (getf attr :read-only) nil)
    (unless object
      (setf (getf attr :value) (getf attr :default)))
    (apply #'call-next-method widget object attr)))

(defun make-new-widget (formatter object)
  (make-instance 'new-widget :formatter formatter :object object
                             :skip-sections-p t))

(defun object-new-page-attributes (formatter object)
  (let ((widget (make-new-widget formatter object)))
    (render widget)
    widget))
