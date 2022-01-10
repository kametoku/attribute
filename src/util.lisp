(in-package :attribute)

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
