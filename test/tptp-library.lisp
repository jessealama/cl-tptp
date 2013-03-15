
(in-package :cl-tptp-test)

(defun define-parse-test (problem)
  (let ((problem-sym (intern problem :cl-tptp-test))
	(section (subseq problem 0 3)))
    (let ((section-sym (intern section :cl-tptp-test)))
      (eval (list 'define-test
		  problem-sym
		  (list :tag section-sym)
		  (list 'assert-true
			(list 'cl-tptp:parse-released-tptp
			      problem)))))))

(dolist (section (cl-tptp:tptp-problem-sections))
  (let ((problems (cl-tptp:tptp-problems-in-section section)))
    (dolist (problem problems)
      (define-parse-test problem))))
