(defsystem "attribute"
  :version "0.1.0"
  :author "Tokuya Kameshima"
  :license "LLGPL"
  :depends-on ("alexandria"
               "cl-ppcre"
               "weblocks"

               ;; not available from Ultralisp site
               "cl-fomantic" ; https://github.com/kametoku/cl-fomantic
               "tkutil"      ; https://github.com/kametoku/tkutil
               )
  :components ((:module "src"
                :components ((:file "attribute"))))
  :description ""
  :in-order-to ((test-op (test-op "attribute/tests"))))

(defsystem "attribute/tests"
  :author ""
  :license ""
  :depends-on ("attribute"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "attribute"))))
  :description "Test system for attribute"
  :perform (test-op (op c) (symbol-call :rove :run c)))
