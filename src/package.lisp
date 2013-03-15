
(in-package :cl-user)

(defpackage #:cl-tptp
  (:nicknames #:tptp)
  (:use #:cl)
  (:use #:yacc)
  (:import-from #:named-readtables
		#:in-readtable)
  (:import-from #:cl-fad
		#:pathname-as-directory
		#:pathname-as-file
		#:directory-exists-p
		#:file-exists-p
		#:list-directory)
  (:import-from #:alexandria
		#:length=)

  (:export #:tptp-problem-sections
	   #:tptp-problems-in-section
	   #:parse-released-tptp))
