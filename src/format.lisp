(in-package :attribute)

(defparameter *date-time-format*
  '(:year #\- (:month 2) #\- (:day 2) #\  (:hour 2) #\: (:min 2) #\: (:sec 2)))

(defparameter *date-format* '(:year #\- (:month 2) #\- (:day 2)))

(defparameter *month-format* '(:year #\- (:month 2)))

(defun format-date-time (timestamp &key (format *date-time-format*))
  (when timestamp
    (local-time:format-timestring nil timestamp :format format)))

(defun format-date (timestamp &key (format *date-format*))
  (when timestamp
    (local-time:format-timestring nil timestamp :format format)))

(defun format-month (timestamp &key (format *month-format*))
  (when timestamp
    (local-time:format-timestring nil timestamp :format format)))

(defparameter *date-formatters*
  (list #'format-date-time #'format-date #'format-month))

(defun format-currency (amount &key (currency "JPY"))
  (when amount
    (format nil "~:D ~A" amount currency)))

(defun format-small-image (url)
  (when url
    (values (reblocks/html:with-html-string
              (:img :class "ui small image" :src url))
            t)))
