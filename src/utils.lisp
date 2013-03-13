
(in-package :cl-tptp)

(defgeneric symbolify-here (thing))

(defmethod symbolify-here ((thing symbol))
  (symbolify-here (symbol-name thing)))

(defmethod symbolify-here ((thing string))
  (intern thing (find-package :cl-tptp)))
