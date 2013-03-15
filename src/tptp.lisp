
(in-package :cl-tptp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tptp-formula ()
  ((name
    :initarg :name
    :initform (error "An fof needs a name.")
    :accessor name)
   (role
    :initarg :role
    :accessor role
    :initform (error "An fof needs a role."))
   (formula
    :initarg :formula
    :accessor formula
    :initform (error "An fof needs a formula."))
   (source
    :initarg :source
    :accessor source)
   (optional-info
    :initarg :optional-info
    :accessor optional-info)))

(defmethod initialize-instance :after ((x tptp-formula) &rest initargs &key &allow-other-keys)
  "Ensure that if X's optional-info slot is set, then its source slot is also set."
  (declare (ignore initargs))
  (when (slot-boundp x 'optional-info)
    (unless (slot-boundp x 'source)
      (error "A TPTP formula whose optional-info slot is bound must also have its source slot bound."))))

(defclass fof (tptp-formula)
  nil)

(defclass cnf (tptp-formula)
  nil)

(defclass internal-source (tptp-source)
  nil)

(defclass external-source (tptp-source)
  nil)

(defmethod print-object ((x tptp-formula) stream)
  (with-slots (name role formula)
      x
    (if (slot-boundp x 'source)
	(let ((source (source x)))
	  (if (slot-boundp x 'optional-info)
	      (let ((optional-info (optional-info x)))
		(format stream "(~a, ~a, ~a, ~a, ~a)." name role formula source optional-info))
	      (format stream "(~a, ~a, ~a, ~a)." name role formula source)))
	(format stream "(~a, ~a, ~a)." name role formula))))

(defmethod print-object ((x fof) stream)
  (format stream "fof")
  (call-next-method))

(defmethod print-object ((x cnf) stream)
  (format stream "fof")
  (call-next-method))

(defun sort-formula-list (formula-list)
  (let ((sorted (sort formula-list #'string< :key #'name)))
    sorted))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Includes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass include-instruction ()
  ((file
    :accessor file
    :initarg :file
    :initform (error "An include instruction requires a file name."))
   (selection
    :accessor selection
    :initarg :selection
    :type list
    :initform nil)))

(defmethod print-object ((include include-instruction) stream)
  (with-slots (file selection)
      include
    (if selection
	(format stream "include(~a,[~{~a~^,~}])." file selection)
	(format stream "include(~a)." file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expanding includes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric expand-include (include root-dir))

(defmethod expand-include ((include include-instruction) root-dir)
  (with-slots (selection file)
      include
    (when (null selection)
      (return-from expand-include nil))
    (let ((real-file nil)
	  (new-formulas nil))
      (cond ((file-exists-p file)
	     (setf real-file file))
	    ((directory-exists-p root-dir)
	     (let ((file-in-dir (merge-pathnames file root-dir)))
	       (if (file-exists-p file-in-dir)
		   (setf real-file file-in-dir)
		   (error "No file found at '~a', nor could we find '~a'." (namestring file) (namestring file-in-dir)))))
	    ((null root-dir)
	     (error "No file at '~a', and no root directory was supplied." (namestring file)))
	    (t
	     (error "No file at '~a', and to boot a bogus directory (~a) was supplied." (namestring file) root-dir)))
      (let ((included-db (handler-case (parse-tptp real-file)
			   (error () nil))))
	(unless included-db
	  (error "Error parsing '~a' as a TPTP file." (namestring real-file)))
	(setf included-db (expand-includes included-db))
	(loop
	   for selected in selection
	   for corresponding = (formula-with-name included-db
						  selected)
	   do
	     (when (null corresponding)
	       (error "There is no formula in the TPTP problem '~a' with the name '~a'." (namestring real-file) selected))
	     (push corresponding new-formulas)
	   finally
	     (return (reverse new-formulas)))))))

(defun tptp-directory ()
  (let ((env (getenv "TPTP")))
    (when env
      (pathname-as-directory (pathname env)))))

(defun tptp-problems-subdirectory (&optional tptp-directory)
  (unless tptp-directory
    (setf tptp-directory (tptp-directory)))
  (when (stringp tptp-directory)
    (setf tptp-directory (pathname tptp-directory)))
  (when (and tptp-directory
	     (directory-exists-p tptp-directory))
    (merge-pathnames "Problems/" tptp-directory)))

(defun tptp-problem-sections (&optional tptp-directory)
  (unless tptp-directory
    (setf tptp-directory (tptp-directory)))
  (let ((tptp-problems (tptp-problems-subdirectory tptp-directory)))
    (unless tptp-problems
      (error "Unable to compute the TPTP problems directory."))
    (unless (directory-exists-p tptp-problems)
      (error "No such directory~%~%  ~a~%" tptp-problems))
    (let ((ls (list-directory tptp-problems)))
      (let ((subdirs (remove-if-not #'directory-exists-p ls)))
	(let ((subdirs-as-files (mapcar #'pathname-as-file subdirs)))
	  (mapcar #'pathname-name subdirs-as-files))))))

(defgeneric tptp-problems-in-section (section &optional tptp-directory)
  (:documentation "The problems (as strings) within the problem section SECTION relative to the TPTP directory TPTP-DIRECTORY.  If TPTP-DIRECTORY is NIL, the environment variable TPTP will be consulted."))

(defmethod tptp-problems-in-section ((section symbol) &optional tptp-directory)
  (tptp-problems-in-section (symbol-name symbol) tptp-directory))

(defmethod tptp-problems-in-section :before ((section string) &optional tptp-directory)
  (unless (length= section 3)
    (error "The name of TPTP problem sections should be exactly 3 ASCII characters (e.g., 'LAT', 'ALG', etc.).")))

(defmethod tptp-problems-in-section ((section string) &optional tptp-directory)
  (let ((problems-dir (tptp-problems-subdirectory tptp-directory)))
    (unless problems-dir
      (error "Unable to compute the problems subdirectory."))
    (unless (pathnamep problems-dir)
      (error "Unable to compute the problems subdirectory (it is not NIL and also not a pathname object)."))
    (unless (directory-exists-p problems-dir)
      (error "No such directory~%~%  ~a~%" (namestring problems-dir)))
    (let ((section-dir (merge-pathnames (format nil "~a/" section)
					problems-dir)))
      (unless (directory-exists-p section-dir)
	(error "No such directory~%~%  ~a~%" (namestring section-dir)))
      (let ((ls (list-directory section-dir)))
	(mapcar #'pathname-name ls)))))
