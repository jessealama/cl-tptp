
(in-package :cl-tptp)

(defun stringify (x)
  (format nil "~a" x))

(defun hash-table-keys (table)
  (loop for k being the hash-keys in table collect k))

(defgeneric symbolify-here (thing))

(defmethod symbolify-here ((thing symbol))
  (symbolify-here (symbol-name thing)))

(defmethod symbolify-here ((thing string))
  (intern thing (find-package :cl-tptp)))

(defgeneric getenv (environment-variable)
  (:documentation "Get the value of ENVIRONMENT-VARIABLE from the environment.  If ENVIRONMENT-VARIABLE is undefined, return NIL."))

(defmethod getenv ((variable symbol))
  (getenv (symbol-name variable)))

(defmethod getenv ((variable string))
  (when (string= variable "")
    (error "The empty string is not the name of an environment variable."))
  (when (or (find #\Space variable)
	    (find #\Newline variable)
	    (find #\Tab variable))
    (error "Whitespace is not allowed in (names of) environment variables."))
  #+ccl
  (ccl:getenv variable)
  #+sbcl
  (sb-posix:getenv variable)
  #-(or ccl sbcl)
  (error "We know how to extract values of environment variables only with CCL and SBCL."))
