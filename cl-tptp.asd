(asdf:defsystem #:cl-tptp
  :description "Work with TPTP files in Common Lisp."
  :version "0.1"
  :license ""
  :author "Jesse Alama <jesse.alama@gmail.com>"
  :maintainer "Jesse Alama <jesse.alama@gmail.com>"
  :depends-on (:yacc :named-readtables :cl-fad :alexandria)
  :serial t
  :components ((:file "src/package")
	       (:file "src/utils")
	       (:file "src/expressions")
	       (:file "src/tptp")
	       (:file "src/db")
	       (:file "src/parse")))
