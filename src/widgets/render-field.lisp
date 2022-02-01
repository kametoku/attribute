(in-package :attribute)

(defun format-field-value (value format)
  (cond ((stringp format) (format nil format value))
        ((functionp format) (funcall format value))
        (t value)))

(defmacro with-format-field-value ((val raw-p) value format &body body)
  (let ((gval (gensym)))
    `(multiple-value-bind (,gval ,raw-p)
         (format-field-value ,value ,format)
       (let ((,val (tkutil:ensure-string ,gval)))
         ,@body))))

(defgeneric %render-field (tag type &rest attr))

(defmethod %render-field ((tag (eql :td)) type &key value format href target style
                          &allow-other-keys)
  (with-format-field-value (val raw-p) value format
    (with-html
      (:td :style style
           (cond ((not (tkutil:blankp href))
                  (:a :href href :target target
                      (if raw-p (:raw val) val)
                      (when target
                        (:sup (:raw "&nbsp;")
                              (:i :class "external alternate icon")))))
                 (raw-p (:raw val))
                 (t val))))))

(defmethod %render-field ((tag (eql :td)) (type (eql :checkbox)) &key value style
                          &allow-other-keys)
  "Render a checkbox for read-only view."
  (with-html
    (:td :style style
         (if value
             (:i :class "toggle on icon")
             (:i :class "toggle off icon")))))

(defmethod %render-field ((tag (eql :input)) (type (eql :checkbox))
                          &key name value read-only &allow-other-keys)
  (let ((widget (fui.modules:make-checkbox-widget
                 :name name :value value :read-only read-only)))
    (render widget)
    widget))

(defmethod %render-field ((tag (eql :input)) (type (eql :textarea))
                          &key name value format maxlength read-only &allow-other-keys)
  (with-format-field-value (val raw-p) value format
    (when raw-p
      (error "raw-p could not be non-nil for textarea field."))
    (with-html
      (:textarea :rows 4 :name name :id name
                 :maxlength maxlength :disabled read-only
                 val))))

(defmethod %render-field ((tag (eql :input)) (type (eql :dropdown))
                          &key name value placeholder datalist &allow-other-keys)
  (when (functionp datalist)
    (setf datalist (funcall datalist)))
  (let ((widget (fui.modules:make-selection-dropdown-widget
                 :name name :value value :placeholder placeholder
                 :dropdown-items datalist)))
    (reblocks/widget:render widget)
    widget))

(defmethod %render-field ((tag (eql :input)) (type (eql :calendar))
                          &key name value placeholder required &allow-other-keys)
  (let ((widget (fui.modules:make-calendar-widget
                 :name name :value value :placeholder placeholder
                 :required required)))
    (reblocks/widget:render widget)
    widget))

(defmethod %render-field ((tag (eql :input)) type
                          &key name value format placeholder required autofocus
                            minlength maxlength min max datalist datalist-id read-only
                          &allow-other-keys)
  (with-format-field-value (val raw-p) value format
    (when raw-p
      (error "raw-p could not be non-nil for input field."))
    (with-html
      (:input :type type :name name :value val
              :placeholder placeholder :required required :autofocus autofocus
              :minlength minlength :maxlength maxlength :min min :max max
              :list datalist-id
              :id name :disabled read-only
              (when datalist
                (:datalist :id datalist-id datalist))))))


(defun render-field (attr &key object (tag :td))
  (unless (consp attr) (setf attr (list attr)))
  (destructuring-bind (&key value href (object object) skip-if-nil
                         name (type "text") datalist read-only
                       &allow-other-keys)
      attr
    (when (and skip-if-nil (not object))
      (with-html
        (:tag :name tag :disabled (or read-only (not name))))
      (return-from render-field))
    (let* ((value (tkutil:ensure-value value object))
           (type (tkutil:ensure-keyword type))
           (read-only (or read-only (not name)))
           (href (if (and (functionp href) object)
                     (funcall href object)
                     href))
           (datalist-id (and datalist (format nil "~A-datalist" name))))
      (apply #'%render-field tag type
             :value value :read-only read-only :href href :datalist-id datalist-id attr))))
