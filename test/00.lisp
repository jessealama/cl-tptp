
(in-package :cl-tptp-test)

(define-test "package"
  (assert-true (find-package "cl-tptp")))
