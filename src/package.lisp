
(in-package :cl-user)

(defpackage #:cl-tptp
  (:nicknames #:tptp)
  (:use #:cl)
  (:use #:yacc)
  (:import-from #:named-readtables
		#:in-readtable)
  (:import-from #:cl-fad
		#:pathname-as-directory
		#:directory-exists-p
		#:file-exists-p)
  (:import-from #:alexandria
		#:length=))
