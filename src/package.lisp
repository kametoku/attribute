(defpackage :attribute
  (:use :cl)
  (:import-from :reblocks/widget
                :defwidget
                :render
                :update)
  (:import-from :reblocks/html
                :with-html)
  (:shadow #:formatter)
  (:export #:attributes
           #:status-filter
           #:secondary-filter
           #:attrs
           #:with-attrs
           #:attr

           #:format-date-time
           #:format-date
           #:format-month
           #:format-currency
           #:format-small-image

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
