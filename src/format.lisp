(in-package :attribute)

(defparameter *date-time-format*
  '(:year #\- (:month 2) #\- (:day 2) #\  (:hour 2) #\: (:min 2) #\: (:sec 2)))

(defparameter *date-format* '(:year #\- (:month 2) #\- (:day 2)))

(defun format-date-time (timestamp)
  (when timestamp
    (local-time:format-timestring nil timestamp :format *date-time-format*)))

(defun format-date (timestamp)
  (when timestamp
    (local-time:format-timestring nil timestamp :format *date-format*)))

(defparameter *date-formatters*
  (list #'format-date #'format-date-time))

(defun format-currency (amount &key (currency "JPY"))
  (when amount
    (format nil "~:D ~A" amount currency)))

(defun format-small-image (url)
  (when url
    (values (reblocks/html:with-html-string
              (:img :class "ui small image" :src url))
            t)))
