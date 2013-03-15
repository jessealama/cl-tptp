(asdf:defsystem #:cl-tptp-test
  :description "Testing cl-tptp."
  :version "0.1"
  :license ""
  :author "Jesse Alama <jesse.alama@gmail.com>"
  :maintainer "Jesse Alama <jesse.alama@gmail.com>"
  :depends-on (:lisp-unit :cl-tptp)
  :serial t
  :components ((:file "test/package")
	       (:file "test/00")
	       (:file "test/tptp-library")))
