
(in-package :cl-user)

(defpackage #:cl-tptp
  (:nicknames #:tptp)
  (:use #:cl)
  (:use #:yacc)
  (:import-from #:named-readtables
		#:in-readtable))
