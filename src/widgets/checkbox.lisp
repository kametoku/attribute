(in-package :attribute)

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
                    :onclick (reblocks/actions:make-js-action
                              (lambda (&key &allow-other-keys)
                                (toggle widget)))
                    :value "1" :id name :disabled read-only)
            (:label "")))))
