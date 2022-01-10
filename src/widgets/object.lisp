(in-package :attribute)

(defwidget object-widget ()
  ((formatter :initarg :formatter
              :initform #'identity
              :reader formatter)
   (object :initarg :object
           :initform nil
           :reader object)
   (skip-sections-p :initarg :skip-sections-p
                    :initform nil
                    :reader skip-sections-p)))

(defmethod render-attr ((widget object-widget) object &rest attr)
  (render-field attr :object object))

(defmethod render-attrs ((widget object-widget) &rest args)
  (with-attrs (section icon object attrs) args
    (unless (functionp (car attrs)) ; do not run the function by default.
      (loop for attr in attrs
            for result = (apply #'render-attr widget object attr)
            when result
              collect result))))

(defmethod render-object ((widget object-widget))
  "Call `render-object' directly when you need the results of `render-attrs'.
On the other hand, `reblocks/widget:render' would not return any valus."
  (let ((attributes (unexpand (funcall (formatter widget) (object widget)))))
    (when (skip-sections-p widget)
      (setf (cdr attributes) nil))
    (loop for attrs in attributes
                  nconc (tkutil:ensure-list
                         (apply #'render-attrs widget attrs)))))

(defmethod render ((widget object-widget))
  (render-object widget))

(defun section-title (section &optional icon)
  (when section
    (with-html
      (:h3 :class "ui secondary header"
           (when icon (:i :class (format nil "~A icon" icon)))
           (:div :class "content" section)))))

