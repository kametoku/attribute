(defpackage :attribute
  (:use :cl)
  (:import-from :weblocks/widget
                :defwidget
                :render
                :update)
  (:import-from :weblocks/html
                :with-html)
  (:shadow #:formatter)
  (:export #:attributes
           #:status-filter
           #:secondary-filter
           #:format-date-time
           #:format-date
           #:format-currency
           #:format-small-image
           #:attrs
           #:with-attrs
           #:attr
;;            #:lazy-lambda
;;            #:format-field-value
;;            #:format-field
           #:build-columns
           #:object-list-page-attributes
           #:object-page-attributes
           #:input-widgets
           #:make-edit-widget
           #:make-new-widget
           #:write-csv))
(in-package :attribute)

(defgeneric attributes (object))

(defgeneric status-filter (object-type)
  (:method (object-type) nil))

(defgeneric secondary-filter (object-type)
  (:method (object-type) nil))

(defparameter *date-time-format*
  '(:year #\- (:month 2) #\- (:day 2) #\  (:hour 2) #\: (:min 2) #\: (:sec 2)))

(defparameter *date-format* '(:year #\- (:month 2) #\- (:day 2)))

(defun format-date-time (timestamp)
  (when timestamp
    (local-time:format-timestring nil timestamp :format *date-time-format*)))

(defun format-date (timestamp)
  (when timestamp
    (local-time:format-timestring nil timestamp :format *date-format*)))

(defun format-currency (amount &key (currency "JPY"))
  (when amount
    (format nil "~:D ~A" amount currency)))

(defun format-small-image (url)
  (when url
    (values (weblocks/html:with-html-string
              (:img :class "ui small image" :src url))
            t)))

(defun car-depth (list)
  "Returns the depth of the list car part."
  (if (atom list) 0
      (1+ (car-depth (car list)))))

(defun expand (list)
  (if (< (car-depth list) 3) list
      (nconc (expand (car list))
             (expand (cdr list)))))

(defun unexpand (list)
  (when (< (car-depth list) 3)
    (setf list (list list)))
  (remove nil list))

(defmacro attrs ((&key section icon object) &rest attrs)
  `(list
    (list :section ,section :icon ,icon :object (lambda () ,object))
    ,@attrs))

(defmacro with-attrs ((section icon object attrs) args &body body)
  `(destructuring-bind ((&key section icon object) &rest attrs)
       ,args
     (let ((,section section)
           (,icon icon)
           (,object (tkutil:ensure-value object))
           (,attrs attrs))
       (declare (ignorable ,section ,icon ,attrs))
       (unless (and section (not object))
         ,@body))))

(defmacro lazy-lambda ((&rest args) &body body)
  `(let ,(mapcar (lambda (arg) (list arg arg)) args)
     (lambda ()
       ,@body)))

(defun attr (label value &rest args &key colname href target style format object skip-if-nil
              name type required autofocus placeholder minlength maxlength min max datalist default
              edit-only read-only hide-in-new insignificant)
  (declare (ignorable colname href target style format object skip-if-nil
                      name type required autofocus placeholder minlength maxlength min max datalist default
                      edit-only read-only hide-in-new insignificant))
  (apply #'list :label label :value value args))

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
  (with-html
    (if value
        (:i :class "toggle on icon")
        (:i :class "toggle off icon"))))


(defwidget checkbox ()
  ((name :initarg :name :initform nil :reader name)
   (value :initarg :value :initform nil :accessor value)
   (read-only :initarg :read-only :initform nil :reader read-only)))

(defmethod fui.modules:name ((widget checkbox))
  (name widget))

(defmethod fui.modules:value ((widget checkbox))
  (value widget))

(defmethod toggle ((widget checkbox))
  (setf (value widget) (not (value widget)))
  (update widget))

(defmethod render ((widget checkbox))
  (let ((name (name widget))
        (value (value widget))
        (read-only (read-only widget)))
    (with-html
      (:div :class "ui toggle checkbox"
            (:input :type "checkbox" :name name :checked value
                    :onclick (weblocks/actions:make-js-action
                              (lambda (&key &allow-other-keys)
                                (toggle widget)))
                    :value "1" :id name :disabled read-only)
            (:label "")))))

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
                (weblocks/widget:render widget)
                widget))
             (t
              (:input :type type :name name :value val
                      :placeholder placeholder :required required :autofocus autofocus
                      :minlength minlength :maxlength maxlength :min min :max max
                      :list datalist-id
                      :id name :disabled read-only
                      (when datalist
                        (:datalist :id datalist-id datalist)))))))))))

(defparameter *date-formatters*
  (list #'format-date #'format-date-time))


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
On the other hand, `weblocks/widget:render' would not return any valus."
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


(defwidget list-widget (object-widget) ())

(defmethod weblocks/widget:get-html-tag ((widget list-widget))
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


(defwidget property-widget (object-widget) ())

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


(defwidget edit-widget (object-widget)
  ((input-widgets :initform nil :accessor input-widgets)))

(defmethod weblocks/widget:get-css-classes ((widget edit-widget))
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


(defwidget new-widget (edit-widget) ())

;; (defmethod weblocks/widget:get-css-classes ((widget new-widget))
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


(defwidget csv-header-widget (object-widget) ())

(defmethod render-attr ((widget csv-header-widget) object &rest attr
                        &key label &allow-other-keys)
  (declare (ignore attr))
  label)

(defun make-csv-header-widget (formatter)
  (make-instance 'csv-header-widget :formatter formatter))

(defun csv-header-row (row-formatter)
  (render (make-csv-header-widget formatter)))


(defwidget csv-data-widget (object-widget) ())

(defmethod render-attr ((widget csv-data-widget) %object &rest attr
                        &key value format (object %object) &allow-other-keys)
  (declare (ignore attr))
  (let ((value (tkutil:ensure-value value object)))
    (format-field-value value format)))

(defun csv-data-row (row-formatter object)
  (render-object row-formatter object #'object-attr-value))
