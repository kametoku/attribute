(in-package :attribute)

(defun format-field-value (value format)
  (cond ((stringp format) (format nil format value))
        ((functionp format) (funcall format value))
        (t value)))

(defun render-text (val &optional href target raw-p)
  (with-html
    (cond ((not (tkutil:blankp href))
           (:a :href href :target target
               (if raw-p (:raw val) val)
               (when target
                 (:sup (:raw "&nbsp;")
                       (:i :class "external alternate icon")))))
          (raw-p (:raw val))
          (t val))))

(defun render-checkbox (value)
  "Render a checkbox for read-only view."
  (with-html
    (if value
        (:i :class "toggle on icon")
        (:i :class "toggle off icon"))))

(defun render-field (attr &key object (tag :td))
  (unless (consp attr) (setf attr (list attr)))
  (destructuring-bind (&key label value colname href target style format (object object)
                         skip-if-nil
                         name (type "text") required autofocus
                         placeholder minlength maxlength min max datalist default
                         edit-only read-only hide-in-new insignificant)
      attr
    (declare (ignore label colname default edit-only hide-in-new insignificant))
    (when (and skip-if-nil (not object))
      (with-html
        (:tag :name tag :disabled (or read-only (not name))))
      (return-from render-field))
    (let* ((value (tkutil:ensure-value value object))
           (%val (multiple-value-list (format-field-value value format)))
           (val (tkutil:ensure-string (car %val)))
           (raw-p (cadr %val))
           (read-only (or read-only (not name)))
           (datalist-id (and datalist
                             (format nil "~A-datalist" name))))
      (when (and (functionp href) object)
        (setf href (funcall href object)))
      (ecase tag
        (:td
         (with-html
           (cond ((equal type "checkbox")
                  (:td :style style (render-checkbox value)))
                 (t
                  (:td :style style (render-text val href target raw-p))))))
        (:input
         (with-html
           (cond
             ((equal type "checkbox")
              (let ((widget (make-instance 'checkbox
                                           :name name :value value
                                           :read-only read-only)))
                (render widget)
                widget))
             ((equal type "textarea")
              (:textarea :rows 4 :name name :id name
                         :maxlength maxlength :disabled read-only
                         val))
             ((equal type "dropdown")
              (when (functionp datalist) (setf datalist (funcall datalist)))
              (let ((widget (fui.modules:make-selection-dropdown-widget
                             :name name :value value :placeholder placeholder
                             :dropdown-items datalist)))
                (reblocks/widget:render widget)
                widget))
             (t
              (:input :type type :name name :value val
                      :placeholder placeholder :required required :autofocus autofocus
                      :minlength minlength :maxlength maxlength :min min :max max
                      :list datalist-id
                      :id name :disabled read-only
                      (when datalist
                        (:datalist :id datalist-id datalist)))))))))))
