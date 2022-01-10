(in-package :attribute)

(defgeneric attributes (object))

(defgeneric status-filter (object-type)
  (:method (object-type) nil))

(defgeneric secondary-filter (object-type)
  (:method (object-type) nil))

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

;; (defmacro lazy-lambda ((&rest args) &body body)
;;   `(let ,(mapcar (lambda (arg) (list arg arg)) args)
;;      (lambda ()
;;        ,@body)))

(defun attr (label value &rest args &key colname href target style format object skip-if-nil
              name type required autofocus placeholder minlength maxlength min max datalist default
              edit-only read-only hide-in-new insignificant)
  (declare (ignorable colname href target style format object skip-if-nil
                      name type required autofocus placeholder minlength maxlength min max datalist default
                      edit-only read-only hide-in-new insignificant))
  (apply #'list :label label :value value args))
