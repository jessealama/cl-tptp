
(in-package :cl-tptp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TPTP databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tptp-db ()
  ((formulas :type list
	     :initarg :formulas
	     :accessor formulas
	     :initform nil)
   (path
    :type (or null pathname)
    :accessor path
    :initform nil
    :initarg :path)))

(defmethod print-object ((problem tptp-db) stream)
  (format stream "~{~a~^~%~}" (formulas problem)))

(defun problem-directory (tptp-db)
  (with-slots (path)
      tptp-db
    (when (pathnamep path)
      (directory-namestring path))))

(defmethod signature ((formula tptp-formula))
  (signature (formula formula)))

(defmethod signature ((tptp tptp-db))
  (reduce #'merge-signatures
	  (mapcar #'signature
		  (mapcar #'formula
			  (formulas (expand-includes tptp))))))

(defclass derivability-problem (tptp-db)
  ((conjecture
    :initarg :conjecture
    :accessor conjecture
    :initform (error "To specify a derivability problem, a conjecture must be supplied."))))

(defmethod print-object ((problem derivability-problem) stream)
  (let ((conjecture (conjecture problem))
	(formulas (formulas problem)))
    (format stream "~a" conjecture)
    (when formulas
      (terpri stream)
      (format stream "~{~a~^~%~}" formulas))))

(defmethod initialize-instance :after ((problem derivability-problem) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (when (conjecture-formula (premises problem))
    (error "Some non-conjecture formula has the TPTP status 'conjecture'."))
  (loop
     :initially (setf (role (conjecture problem)) "conjecture")
     :for formula in (formulas problem)
     :for role = (role formula)
     :unless (string= role "conjecture") :do (setf (role formula) "axiom")
     :finally (return problem)))

(defgeneric make-derivability-problem (formulas))

(defmethod make-derivability-problem ((formulas tptp-db))
  (let ((conjecture (conjecture-formula formulas)))
    (if conjecture
	(make-instance 'derivability-problem
		       :formulas (non-conjecture-formulas formulas)
		       :conjecture conjecture
		       :path (path formulas))
	(error "There is no conjecture formula in ~a." formulas))))

(defmethod make-derivability-problem ((formulas null))
  (error "The empty list does not contain a conjecture formula."))

(defmethod make-derivability-problem ((formulas list))
  (let ((conjecture (find "conjecture" formulas :test #'string= :key #'role))
	(non-conjecture-formulas (remove-if #'(lambda (formula)
						(string= (role formula) "conjecture"))
					    formulas)))
    (if conjecture
	(make-instance 'derivability-problem
		       :formulas non-conjecture-formulas
		       :conjecture conjecture)
	(error "No conjecture formula found in ~{~a~%~}" formulas))))

(defmethod make-derivability-problem ((problem pathname))
  (make-derivability-problem (parse-tptp problem)))

(defmethod render ((formulas list))
  (if formulas
      (format nil "~{~a~%~}" (mapcar #'render formulas))
      (format nil "(empty formula list)")))

(defmethod render ((problem tptp-db))
  (render (formulas problem)))

(defmethod kowalski ((l null))
  nil)

(defmethod kowalski ((l list))
  (mapcar #'kowalski l))

(defmethod render ((problem derivability-problem))
  (with-output-to-string (s)
    (dolist (formula (formulas problem))
      (format s "~a" (render formula))
      (terpri s))
    (format s "~a" (render (conjecture problem)))
    (terpri s)))

(defgeneric proper-formulas (problem))

(defmethod proper-formulas ((problem tptp-db))
  (mapcar #'formula (formulas problem)))

(defgeneric conjecture-formula (thing)
  (:documentation "The conjecture formula in THING."))

(defmethod conjecture-formula ((db tptp-db))
  (find "conjecture" (formulas db) :test #'string= :key #'role))

(defmethod conjecture-formula ((x null))
  nil)

(defmethod conjecture-formula ((l list))
  (find "conjecture" l :test #'string= :key #'role))

(defun has-conjecture-p (problem)
  (not (null (conjecture-formula problem))))

(defun remove-conjecture (problem)
  (make-instance 'tptp-db
		 :formulas (remove (conjecture-formula problem)
				   (formulas problem))))

(defgeneric remove-formula (formulas formula))

(defmethod remove-formula ((formulas tptp-db) (formula-name string))
  "Remove any formula in FORMULAS whose name is FORMULA-NAME."
  (make-instance 'tptp-db
		 :formulas (remove formula-name
				   (formulas formulas)
				   :test #'string=
				   :key #'name)))

;; (defmethod remove-formula ((problem derivability-problem) (formula tptp-formula))
;;   (let ((name-to-remove (name formula))
;; 	(conjecture-name (name (conjecture problem))))
;;     (if (string= name-to-remove conjecture-name)
;; 	(make-instance 'tptp-db
;; 		       :formulas (formulas problem))
;; 	(make-instance 'derivability-problem
;; 		       :conjecture (conjecture problem)
;; 		       :formulas (remove-if #'(lambda (x) (string= x name-to-remove))
;; 					    (formulas problem)
;; 					    :key #'name)))))

(defmethod remove-formula ((formulas tptp-db) (formula tptp-formula))
  (remove-formula formulas (name formula)))

(defun formulas-with-status (problem status)
  (remove-if-not #'(lambda (stat) (string= stat status))
		 (formulas problem)
		 :key #'role))

(defun statuses-of-formulas (problem)
  (loop
     with statuses = (make-hash-table :test #'equal)
     for formula in (formulas problem)
     for status = (role formula)
     do (setf (gethash status statuses) 0)
     finally (return (hash-table-keys statuses))))

(defun non-conjecture-formulas (problem)
  (remove-if #'(lambda (stat) (string= stat "conjecture"))
	     (formulas problem)
	     :key #'role))

(defgeneric change-status (formula new-status))

(defmethod change-status :around ((formula tptp-formula) status)
  (let ((new-formula (call-next-method)))
    (when (slot-boundp formula 'source)
      (setf (source new-formula) (source formula)))
    (when (slot-boundp formula 'optional-info)
      (setf (optional-info new-formula)
	    (optional-info formula)))
    new-formula))

(defmethod change-status ((formula tptp-formula) new-status)
  (make-instance (class-of formula)
		 :name (name formula)
		 :syntax (role formula)
		 :role new-status
		 :formula (formula formula)))

(defgeneric change-status-of-formula-in (formula problem new-status)
  (:documentation "Change the TPTP status of FORMULA in PROBLEM to NEW-STATUS."))

(defmethod change-status-of-formula-in ((formula string)
					(problem pathname)
					(new-status string))
  (change-status-of-formula-in formula (parse-tptp problem) new-status))

(defmethod change-status-of-formula-in ((formula string)
					(problem tptp-db)
					(new-status string))
  (let ((formula-in-problem (formula-with-name problem formula)))
    (if formula-in-problem
	(let ((other-formulas (remove-if #'(lambda (name)
					     (string= name formula))
					 (formulas problem)
					 :key #'name)))
	  (let ((new-formula (change-status formula-in-problem new-status)))
	    (make-instance 'tptp-db
			   :formulas (cons new-formula
					   other-formulas)))))))

(defmethod change-status-of-formula-in ((formula string)
					(problem derivability-problem)
					(new-status string))
  (if (string= new-status "conjecture")
      (let ((conjecture (conjecture problem)))
	(let ((conjecture-name (name conjecture)))
	  (if (string= conjecture-name formula)
	      problem
	      (error "The given derivability-problem already has a conjecture formula; (by the name ~a), so we cannot change the status of ~a into 'conjecture'." conjecture-name formula))))
      (let ((formula-in-problem (formula-with-name problem formula)))
    (if formula-in-problem
	(let ((other-formulas (remove-if #'(lambda (name)
					     (string= name formula))
					 (formulas problem)
					 :key #'name)))
	  (let ((new-formula (change-status formula-in-problem new-status)))
	    (make-instance 'tptp-db
			   :formulas (cons new-formula
					   other-formulas))))))))

(defun promote-conjecture-to-axiom (problem)
  (let ((conjecture (has-conjecture-p problem)))
    (if conjecture
	(make-instance 'tptp-db
		       :formulas (cons (change-status conjecture "axiom")
				       (non-conjecture-formulas problem)))
	problem)))

(defun formula-names (tptp-db)
  (mapcar #'name (formulas tptp-db)))

(defgeneric formula-with-name (tptp-db name))

(defmethod formula-with-name ((tptp-db tptp-db) (name integer))
  (formula-with-name tptp-db (format nil "~d" name)))

(defmethod formula-with-name ((tptp-db tptp-db) (name symbol))
  (formula-with-name tptp-db (symbol-name name)))

(defmethod formula-with-name ((tptp-db tptp-db) (name string))
  (find name (formulas-w/o-includes tptp-db)
	:test #'string=
	:key #'name))

(defmethod formula-with-name ((tptp-path pathname) name)
  (formula-with-name (parse-tptp tptp-path) name))

(defgeneric premises (problem))

(defmethod premises ((db tptp-db))
  (non-conjecture-formulas db))

(defgeneric restrict-to (db formulas)
  (:documentation "Restrict DB to the formulas in FORMULAS."))

(defmethod restrict-to ((db tptp-db) (formulas list))
  (let ((new-formulas nil))
    (dolist (formula formulas)
      (cond ((stringp formula)
	     (let ((formula-in-db (formula-with-name db formula)))
	       (when formula-in-db
		 (push formula-in-db new-formulas))))
	    ((typep formula 'formula)
	     (let ((formula-in-db (formula-with-name db (name formula))))
	       (when formula-in-db
		 (push formula-in-db new-formulas))))
	    (t
	     (error "Don't know how to handle ~a." formula))))
    (make-instance 'tptp-db
		   :formulas new-formulas)))

(defmethod restrict-to ((problem derivability-problem) (formulas list))
  (let* ((new-formulas nil)
	 (conjecture (conjecture problem))
	 (conjecture-name (name conjecture)))
    (dolist (formula formulas)
      (cond ((stringp formula)
	     (let ((formula-in-db (formula-with-name problem formula)))
	       (when formula-in-db
		 (unless (string= formula conjecture-name)
		   (push formula-in-db new-formulas)))))
	    ((typep formula 'formula)
	     (let ((formula-in-db (formula-with-name problem (name formula))))
	       (when formula-in-db
		 (unless (string= (name formula) conjecture-name)
		   (push formula-in-db new-formulas)))))
	    (t
	     (error "Don't know how to handle ~a." formula))))
    (make-instance 'derivability-problem
		   :conjecture conjecture
		   :formulas new-formulas)))

(defgeneric simplify-justification (tptp))

(defmethod simplify-justification ((tptp-string string))
  (simplify-justification (parse-tptp tptp-string)))

(defmethod simplify-justification ((tptp-path pathname))
  (simplify-justification (parse-tptp tptp-path)))

(defmethod simplify-justification ((tptp-db tptp-db))
  (let* ((formulas (formulas tptp-db))
	 (new-formulas nil)
	 (names (mapcar #'name formulas))
	 (names-table (make-hash-table :test #'equal)))
    (dolist (name names)
      (setf (gethash name names-table) t))
    (dolist (formula formulas)
      (if (slot-boundp formula 'source)
	  (let* ((source (source formula))
		 (earlier-table (make-hash-table :test #'equal))
		 (atoms (flatten-tptp source)))
	    (dolist (atom atoms)
	      (when (gethash atom names-table)
		(unless (gethash atom earlier-table)
		  (setf (gethash atom earlier-table) t))))
	    (let ((new-source (make-instance 'general-list
					     :terms (hash-table-keys earlier-table))))
	      (let ((new-formula (make-instance (class-of formula)
						:name (name formula)
						:role (role formula)
						:formula (formula formula)
						:source new-source)))
		(when (slot-boundp formula 'optional-info)
		  (setf (optional-info new-formula)
			(optional-info formula)))
		(push new-formula new-formulas))))
	  (let ((new-formula (make-instance (class-of formula)
					    :name (name formula)
					    :role (role formula)
					    :formula (formula formula))))
	    (setf (source new-formula)
		  (make-instance 'general-list :terms nil))
	    (push new-formula new-formulas))))
    (make-instance 'tptp-db
		   :formulas (reverse new-formulas))))

(defmethod simplify-justification ((include include-instruction))
  include)

(defmethod kowalski ((db tptp-db))
  (make-instance 'tptp-db
		 :path (path db)
		 :formulas (mapcar #'kowalski (formulas db))))

(defmethod squeeze-quantifiers ((db tptp-db))
  (make-instance 'tptp-db
		 :path (path db)
		 :formulas (mapcar #'squeeze-quantifiers (formulas db))))

(defmethod fofify ((db tptp-db))
  (make-instance 'tptp-db
		 :path (path db)
		 :formulas (mapcar #'fofify (formulas db))))
