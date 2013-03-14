
(in-package :cl-tptp)

(defun stringify (x)
  (format nil "~a" x))

(defgeneric symbolify-here (thing))

(defmethod symbolify-here ((thing symbol))
  (symbolify-here (symbol-name thing)))

(defmethod symbolify-here ((thing string))
  (intern thing (find-package :cl-tptp)))
