
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

(defmethod fofify ((db tptp-db))
  (make-instance 'tptp-db
		 :path (path db)
		 :formulas (mapcar #'fofify (formulas db))))

(defgeneric expand-includes (tptp))

(defmethod expand-includes ((tptp-db pathname))
  (expand-includes (parse-tptp tptp-db)))

(defmethod expand-includes ((tptp-db tptp-db))
  (let ((new-formulas nil)
	(path (path tptp-db))
	(dir (problem-directory tptp-db)))
    (if (null (include-instructions tptp-db))
	tptp-db
	(loop
	   for formula in (formulas tptp-db)
	   do
	     (if (eql (type-of formula) 'include-instruction)
		 (let ((expanded (expand-include formula dir)))
		   (dolist (x expanded)
		     (push x new-formulas)))
		 (push formula new-formulas))
	   finally
	     (return (make-instance 'tptp-db
				    :formulas (reverse new-formulas)
				    :path path))))))

(defun include-instructions (tptp-db)
  (remove-if-not #'(lambda (x) (eql (type-of x) 'include-instruction)) (formulas tptp-db)))

(defgeneric has-include-instruction-p (problem)
  (:documentation "Does PROBLEM contain at least one include instruction?"))

(defmethod has-include-instruction-p ((problem pathname))
  (has-include-instruction-p (parse-tptp problem)))

(defmethod has-include-instruction-p ((problem tptp-db))
  (some #'(lambda (x) (typep x 'include-instruction)) (formulas problem)))

(defun formulas-w/o-includes (tptp-db)
  (remove-if #'(lambda (x) (eql (type-of x) 'include-instruction)) (formulas tptp-db)))

(defgeneric axioms (tptp))

(defmethod axioms ((db tptp-db))
  (remove-if-not #'axiom-p (formulas db)))

(defgeneric axiom-p (formula))

(defmethod axiom-p ((formula tptp-formula))
  (if (slot-boundp formula 'source)
      (let ((source (source formula)))
	(cond ((typep source 'internal-source)
	       nil)
	      ((typep source 'general-list)
	       (null (terms source)))
	      ((integerp source)
	       nil)
	      (t
	       (error "Don't know how to determine whether '~a' is an axiom." formula))))
      t))

(defgeneric fofify (tptp))

(defmethod fofify ((l list))
  (mapcar #'fofify l))

(defmethod fofify ((x null))
  nil)

(defmethod fofify :around ((formula tptp-formula))
  (let ((new-formula (call-next-method)))
    (when (slot-boundp formula 'source)
      (setf (source new-formula) (source formula)))
    (when (slot-boundp formula 'optional-info)
      (setf (optional-info new-formula)
	    (optional-info formula)))
    new-formula))

(defmethod fofify ((x formula))
  (universally-close x))

(defmethod fofify ((formula tptp-formula))
  (make-instance 'fof
		 :name (name formula)
		 :role (role formula)
		 :formula (fofify (formula formula))))

(defgeneric premises (thing))

(defmethod premises ((x null))
  nil)

(defmethod premises ((x inference-record))
  (premises (parents x)))

(defmethod premises ((x null))
  nil)

(defmethod premises ((x general-list))
  (reduce #'append (mapcar #'premises (terms x))))

(defmethod premises ((x list))
  (reduce #'append (mapcar #'premises x)))

(defmethod premises ((x integer))
  (list x))

(defmethod premises ((x string))
  (list x))

(defmethod premises ((x tptp-formula))
  (when (slot-boundp x 'source)
    (premises (source x))))

(defmethod premises ((x atomic-expression))
  nil)

(defun replace-premises (formula new-premises)
  (setf (source formula)
	(make-instance 'atomic-expression
		       :head (intern "inference" :cl-tptp)
		       :arguments (list (make-instance 'atomic-expression
						       :head (intern "unknown" :cl-tptp)
						       :arguments nil)
					(make-instance 'general-list)
					(make-instance 'general-list
						       :terms new-premises))))
  formula)

(defgeneric find-formula (formulas name))

(defmethod find-formula ((formulas null) name)
  nil)

(defmethod find-formula ((formulas list) name)
  (find (stringify name)
	formulas
	:key #'(lambda (x) (stringify (name x)))
	:test #'string=))

(defun extract-problem (formula db)
  (if (slot-boundp formula 'source)
      (let ((premises (premises formula)))
	(let ((premise-formulas (mapcar #'(lambda (x) (formula-with-name db x))
					premises))
	      (conjecture (make-instance 'fof
					 :name (name formula)
					 :role "conjecture"
					 :formula (fofify (formula formula)))))
	  (make-instance 'derivability-problem
			 :conjecture conjecture
			 :formulas (mapcar #'strip-source
					   (mapcar #'fofify premise-formulas)))))
      (make-instance 'derivability-problem
		     :conjecture (make-instance 'fof
						:name (name formula)
						:role "conjecture"
						:formula (fofify (formula formula)))
		     :formulas nil)))

(defgeneric strip-optional-info (tptp-thing))

(defmethod strip-optional-info ((x null))
  nil)

(defmethod strip-optional-info ((l list))
  (mapcar #'strip-optional-info l))

(defmethod strip-optional-info ((db tptp-db))
  (make-instance 'tptp-db
		 :formulas (mapcar #'strip-optional-info (formulas db))))

(defmethod strip-optional-info :around ((x tptp-formula))
  (let ((new-formula (call-next-method)))
    (when (slot-boundp x 'source)
      (setf (source new-formula)
	    (source x)))
    new-formula))

(defmethod strip-optional-info ((x tptp-formula))
  (make-instance (class-of x)
		 :name (name x)
		 :role (role x)
		 :formula (formula x)))


(defgeneric strip-source (tptp-thing))

(defmethod strip-source ((x null))
  nil)

(defmethod strip-source ((l list))
  (mapcar #'strip-source l))

(defmethod strip-source ((db tptp-db))
  (make-instance 'tptp-db
		 :formulas (mapcar #'strip-source (formulas db))))

(defmethod strip-source ((x tptp-formula))
  (make-instance (class-of x)
		 :name (name x)
		 :role (role x)
		 :formula (formula x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Renaming predicate and function symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric rename-symbol (tptp old-name new-name)
  (:documentation "In TPTP, rename the function/predicate symbol whose name is OLD-NAME by NEW-NAME."))

(defmethod rename-symbol ((db tptp-db) old-name new-name)
  (make-instance 'tptp-db
		 :formulas (mapcar #'(lambda (x)
				       (rename-symbol x old-name new-name))
				   (formulas db))))

(defmethod rename-symbol :around ((x tptp-formula) old-name new-name)
  (let ((new-formula (call-next-method)))
    (when (slot-boundp x 'source)
      (setf (source new-formula)
	    (source x)))
    (when (slot-boundp x 'optional-info)
      (setf (optional-info new-formula)
	    (optional-info x)))
    new-formula))

(defmethod rename-symbol ((x tptp-formula) old-name new-name)
  (make-instance (class-of x)
		 :name (name x)
		 :role (role x)
		 :formula (rename-symbol (formula x) old-name new-name)))

(defmethod rename-symbol ((atom atomic-formula) old-name new-name)
  (with-slots (predicate arguments)
      atom
    (make-instance (class-of atom)
		   :predicate (if (string= (stringify predicate)
					   (stringify old-name))
				  (intern (stringify new-name) :cl-tptp)
				  predicate)
		   :arguments (mapcar #'(lambda (x)
					  (rename-symbol x old-name new-name))
				      arguments))))

(defmethod rename-symbol ((atom atomic-expression) old-name new-name)
  (with-slots (head arguments)
      atom
    (make-instance (class-of atom)
		   :head (if (string= (stringify head)
				      (stringify old-name))
			     (intern (stringify new-name) :cl-tptp)
			     head)
		   :arguments (mapcar #'(lambda (x)
					  (rename-symbol x old-name new-name))
				      arguments))))

(defmethod rename-symbol ((neg negation) old-name new-name)
  (make-instance 'negation
		 :argument (rename-symbol (argument neg) old-name new-name)))

(defmethod rename-symbol ((x binary-connective-formula) old-name new-name)
  (make-instance (class-of x)
		 :lhs (rename-symbol (lhs x) old-name new-name)
		 :rhs (rename-symbol (rhs x) old-name new-name)))

(defmethod rename-symbol ((x multiple-arity-connective-formula) old-name new-name)
  (make-instance (class-of x)
		 :items (mapcar #'(lambda (x) (rename-symbol x old-name new-name))
				(items x))))

(defmethod rename-symbol ((gen generalization) old-name new-name)
  (make-instance (class-of gen)
		 :bindings (bindings gen)
		 :matrix (rename-symbol (matrix gen) old-name new-name)))

(defmethod rename-symbol ((eq equation) old-name new-name)
  (make-instance 'equation
		 :lhs (rename-symbol (lhs eq) old-name new-name)
		 :rhs (rename-symbol (rhs eq) old-name new-name)))


(defmethod rename-symbol ((eq disequation) old-name new-name)
  (make-instance 'disequation
		 :lhs (rename-symbol (lhs eq) old-name new-name)
		 :rhs (rename-symbol (rhs eq) old-name new-name)))

(defmethod rename-symbol ((var variable-term) old-name new-name)
  var)

(defmethod flatten-conjunctions/disjunctions ((x null))
  nil)

(defmethod flatten-conjunctions/disjunctions ((l list))
  (mapcar #'flatten-conjunctions/disjunctions l))

(defmethod flatten-conjunctions/disjunctions ((db tptp-db))
  (make-instance 'tptp-db
		 :formulas (mapcar #'flatten-conjunctions/disjunctions (formulas db))))

(defmethod flatten-conjunctions/disjunctions :around ((x tptp-formula))
  (let ((new-formula (call-next-method)))
    (when (slot-boundp x 'source)
      (setf (source new-formula)
	    (source x)))
    (when (slot-boundp x 'optional-info)
      (setf (optional-info new-formula)
	    (optional-info x)))
    new-formula))

(defmethod flatten-conjunctions/disjunctions ((x tptp-formula))
  (make-instance (class-of x)
		 :name (name x)
		 :role (role x)
		 :formula (flatten-conjunctions/disjunctions (formula x))))

(defmethod terms-with-functor (functor-name (db tptp-db))
  (terms-with-functor functor-name (formulas db)))

(defmethod terms-with-functor (functor-name (x tptp-formula))
  (terms-with-functor functor-name (formula x)))

(defgeneric dependency-table (tptp)
  (:documentation "A hash table whose keys are strings naming formulas of TPTP and whose values for a key X is the list of one-step dependencies of X."))

(defmethod dependency-table ((db tptp-db))
  (loop
     :with dep-table = (make-hash-table :test #'equal)
     :for formula :in (formulas db)
     :for name = (stringify (name formula))
     :for premises = (premises formula)
     :do (setf (gethash name dep-table) (mapcar #'stringify premises))
     :finally (return dep-table)))

(defgeneric subproof-terminating-at (tptp step)
  (:documentation "The subproof of TPTP that terminates at STEP."))

(defmethod subproof-terminating-at (tptp (step integer))
  (subproof-terminating-at tptp (format nil "~d" step)))

(defmethod subproof-terminating-at :around ((tptp tptp-db) (step string))
  (if (formula-with-name tptp step)
      (call-next-method)
      (error "No such formula '~a' in the given TPTP database." step)))

(defmethod subproof-terminating-at ((tptp tptp-db) (step string))
  (let ((formulas (formulas tptp))
	(q (make-instance 'q))
	(supporting-formula-table (make-hash-table :test #'equal)))
    (enqueue-at-front q (list (formula-with-name tptp step)))
    (loop
       :until (empty-queue? q)
       :do
       (let ((formula (remove-front q)))
	 (let ((formula-name (name formula)))
	   (unless (gethash formula-name supporting-formula-table)
	     (when (slot-boundp formula 'source)
	       (let ((source (source formula)))
		 (let ((atoms (flatten-tptp source)))
		   (loop
		      :for atom in atoms
		      :for atom-string = (format nil "~a" atom)
		      :when (formula-with-name tptp atom-string)
		      :do
		      (enqueue-at-end q (list (formula-with-name tptp
								 atom-string))))))))
	   (setf (gethash formula-name supporting-formula-table) t))))
    (let ((supporting-formulas (hash-table-keys supporting-formula-table)))
      (let ((sorted-supporting (sort supporting-formulas
				     #'(lambda (formula-1 formula-2)
					 (< (position (stringify formula-1) formulas :key #'(lambda (x) (stringify (name x))) :test #'string=)
					    (position (stringify formula-2) formulas :key #'(lambda (x) (stringify (name x))) :test #'string=))))))
	(make-instance 'tptp-db
		       :formulas (mapcar #'(lambda (name)
					     (formula-with-name tptp name))
					 sorted-supporting))))))

(defmethod universally-close :around ((x tptp-formula))
  (let ((new-formula (call-next-method)))
    (when (slot-boundp x 'source)
      (setf (source new-formula)
	    (source x)))
    (when (slot-boundp x 'optional-info)
      (setf (optional-info new-formula)
	    (optional-info x)))
    new-formula))

(defmethod universally-close ((x tptp-formula))
  (make-instance (class-of x)
		 :name (name x)
		 :role (role x)
		 :formula (universally-close (formula x))))

(defmethod contains-predicate? ((x tptp-formula) predicate)
  (contains-predicate? (formula x) predicate))

(defmethod atomic-formula-p ((x tptp-formula))
  (atomic-formula-p (formula x)))

(defmethod literal-p ((x tptp-formula))
  (literal-p (formula x)))

(defmethod negate :around ((x tptp-formula))
  (let ((new-formula (call-next-method)))
    (when (slot-boundp x 'source)
      (setf (source new-formula) (source x)))
    (when (slot-boundp x 'optional-info)
      (setf (optional-info new-formula) (optional-info x)))
    new-formula))

(defmethod negate ((x tptp-formula))
  (make-instance (class-of x)
		 :name (name x)
		 :role (role x)
		 :formula (negate (formula x))))

(defgeneric definition-p (x)
  (:documentation "Is X a definition?"))

(defmethod definition-p ((x tptp-formula))
  (string= (role x) "definition"))
