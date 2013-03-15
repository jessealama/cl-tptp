
(in-package :cl-tptp)

(defclass expression ()
  nil)

(defclass atomic-expression (expression)
  ((head
    :initarg :head
    :accessor head
    :initform (error "An atomic expression needs a head.")
    :type symbol)
   (arguments
    :initarg :arguments
    :accessor arguments
    :initform nil
    :type list)))

(defmethod print-object ((term atomic-expression) stream)
  (with-slots (head arguments)
      term
    (if (null arguments)
	(format stream "~a" head)
	(format stream "~a(~{~a~^,~})" head arguments))))

(defclass general-list ()
  ((terms
    :type list
    :accessor terms
    :initarg :terms
    :initform nil)))

(defmethod print-object ((l general-list) stream)
  (format stream "[~{~a~^,~}]" (terms l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass term (expression) nil)

(defun term? (thing)
  (typep thing 'term))

(defclass function-term (atomic-expression term)
  nil)

(defun make-function-term (function &rest args)
  (make-instance 'function-term
		 :function function
		 :args args))

(defclass variable-term (atomic-expression term)
  nil)

(defun variable-term-p (x)
  (typep x 'variable-term))

(defmethod print-object ((var variable-term) stream)
  (format stream "~a" (head var)))

(defun variable? (thing)
  (typep thing 'variable-term))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass formula ()
  nil)

(defun formula? (thing)
  (typep thing 'formula))

(defclass atomic-formula (formula)
  ((predicate
    :initarg :predicate
    :accessor predicate)
   (arguments
    :initarg :arguments
    :accessor arguments
    :type list)))

(defgeneric atomic-formula-p (x))

(defmethod atomic-formula-p ((x t))
  nil)

(defmethod atomic-formula-p ((x atomic-formula))
  t)

(defun atomic-formula? (thing)
  (typep thing 'atomic-formula))

(defparameter *nullary-true*
  (make-instance 'atomic-formula
		 :predicate (intern "true")
		 :arguments nil))

(defparameter *nullary-false*
  (make-instance 'atomic-formula
		 :predicate (intern "false")
		 :arguments nil))

(defclass equation (atomic-formula)
  ((lhs
    :accessor lhs
    :initarg :lhs
    :initform (error "An equation needs a left-hand side."))
   (rhs
    :accessor rhs
    :initarg :rhs
    :initform (error "An equation needs a right-hand side."))))

(defmethod initialize-instance :after ((x equation) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (predicate x) (intern "=" :cl-tptp)
	(arguments x) (list (lhs x) (rhs x)))
  x)

(defclass disequation (atomic-formula)
  ((lhs
    :accessor lhs
    :initarg :lhs
    :initform (error "A disequation needs a left-hand side."))
   (rhs
    :accessor rhs
    :initarg :rhs
    :initform (error "A disquation needs a right-hand side."))))

(defmethod initialize-instance :after ((x disequation) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (arguments x)
	(list (lhs x) (rhs x)))
  (setf (predicate x) (intern "!="))
  x)

(defclass unary-connective-formula (composite-formula)
  ((argument :initarg :argument
	     :accessor argument)))

(defclass negation (unary-connective-formula)
  nil)

(defgeneric literal-p (x))

(defmethod literal-p ((x t))
  nil)

(defmethod literal-p ((x negation))
  (atomic-formula-p (argument x)))

(defmethod literal-p ((x atomic-formula))
  t)

(defclass binary-connective-formula (composite-formula)
  ((lhs :initarg :lhs
	:accessor lhs
	:type formula)
   (rhs :initarg :rhs
	:accessor rhs
	:type formula)))

(defclass binary-conjunction (binary-connective-formula)
  nil)

(defclass binary-disjunction (binary-connective-formula)
  nil)

(defclass implication (binary-connective-formula)
  nil)

(defclass reverse-implication (binary-connective-formula)
  nil)

(defclass equivalence (binary-connective-formula)
  nil)

(defclass nonequivalence (binary-connective-formula)
  nil)

;; quantifiers

(defclass generalization (composite-formula)
  ((bindings :initarg :bindings
	     :accessor bindings
	     :type list)
   (matrix :initarg :matrix
	   :accessor matrix
	   :type formula)))

(defclass universal-generalization (generalization)
  nil)

(defclass existential-generalization (generalization)
  nil)

(defun equation? (formula)
  (when (atomic-formula? formula)
    (let ((pred (predicate formula)))
      (string= (symbol-name pred) "="))))

(defmethod print-object ((atom atomic-formula) stream)
  (let ((pred (predicate atom))
	(args (arguments atom)))
    (if (null args)
	(cond ((string= (stringify pred) "true")
	       (format stream "$true"))
	      ((string= (stringify pred) "false")
	       (format stream "$false"))
	      (t
	       (format stream "~a" pred)))
	(format stream "~a(~{~a~^,~})" pred args))))

(defmethod print-object ((x disequation) stream)
  (with-slots (lhs rhs)
      x
    (format stream "~a != ~a" lhs rhs)))

(defmethod print-object ((x equation) stream)
  (with-slots (lhs rhs)
      x
    (format stream "~a = ~a" lhs rhs)))

(defgeneric make-atomic-formula (predicate &rest arguments))

(defun make-equation (lhs rhs)
  (make-atomic-formula '= lhs rhs))

(defclass composite-formula (formula)
  nil)

(defun composite-formula? (formula)
  "Determine whether a formula is non-atomic.

Note that, unlike other predicates such as BINARY-DISJUNCTION? and
UNIVERSAL-GENERALIZATION?, this predicate does not merely test whether
the direct class of its argument is COMPOSITE-FORMULA.  The class
COMPOSITE-FORMULA is defined only to provide a common superclass for
further subclasses, such as BINARY-DISJUNCTION and
UNIVERSAL-GENERALIZATION, that is intended to be disjoint from the
class ATOMIC-FORMULA.  This function expresses that disjointedness."
  (and (formula? formula)
       (not (atomic-formula? formula))))

(defun binary-connective-formula? (thing)
  (typep thing 'binary-connective-formula))

(defmethod print-object :around ((formula binary-connective-formula) stream)
  (format stream "(~A " (lhs formula))
  (call-next-method)
  (format stream " ~A)" (rhs formula)))

(defmethod print-object :around ((formula unary-connective-formula) stream)
  (call-next-method)
  (format stream "~A" (argument formula)))

(defgeneric unnegate (formula))

(defmethod unnegate ((negation negation))
  (argument negation))

(defun negation? (thing)
  (typep thing 'negation))

(defmethod print-object ((neg negation) stream)
  (format stream "~a" #\~))

(defgeneric negate (thing))

(defmethod negate ((formula formula))
  (make-instance 'negation :argument formula))

(defmethod negate ((x null))
  nil)

(defmethod negate ((l list))
  (mapcar #'negate l))

(defclass multiple-arity-connective-formula (composite-formula)
  ((items :initarg :items
	  :accessor items
	  :type list)))

(defun implication? (thing)
  (typep thing 'implication))

(defmethod print-object ((implication implication) stream)
  (format stream "=>"))

(defmethod print-object ((implication reverse-implication) stream)
  (format stream "<="))

(defgeneric make-implication (antecedent consequent))

(defmethod make-implication ((antecedent formula) (consequent formula))
  (make-instance 'implication
		 :lhs antecedent
		 :rhs consequent))

(defgeneric antecedent (formula))
(defgeneric consequent (formula))

(defmethod antecedent ((implication implication))
  (lhs implication))

(defmethod consequent ((implication implication))
  (rhs implication))

(defun equivalence? (thing)
  (typep thing 'equivalence))

(defmethod print-object ((equiv equivalence) stream)
  (format stream "<=>"))

(defun make-equivalence (lhs rhs)
  (make-instance 'equivalence
		 :lhs lhs
		 :rhs rhs))

(defun nonequivalence? (thing)
  (typep thing 'nonequivalence))

(defmethod print-object ((equiv nonequivalence) stream)
  (format stream "<~a>" #\~))

(defun make-nonequivalence (lhs rhs)
  (make-instance 'nonequivalence
		 :lhs lhs
		 :rhs rhs))

;;; disjunctions

(defun binary-disjunction? (thing)
  (typep thing 'binary-disjunction))

(defmethod print-object ((bin-dis binary-disjunction) stream)
  (format stream "|"))

(defgeneric make-binary-disjunction (lhs rhs))

(defclass multiple-arity-disjunction (multiple-arity-connective-formula)
  nil)

(defgeneric disjuncts (formula))

(defmethod disjuncts ((x t))
  (list x))

(defmethod disjuncts ((x binary-disjunction))
  (append (disjuncts (lhs x))
	  (disjuncts (rhs x))))

(defmethod disjuncts ((x multiple-arity-disjunction))
  (disjuncts (items x)))

(defmethod disjuncts ((x null))
  nil)

(defmethod disjuncts ((l list))
  (reduce #'append (mapcar #'disjuncts l)))

(defmethod disjuncts ((x multiple-arity-disjunction))
  (reduce #'append (mapcar #'disjuncts (items x))))

(defun multiple-arity-disjunction? (thing)
  (eql (class-of thing) 'multiple-arity-disjunction))

(defmethod print-object ((mad multiple-arity-disjunction) stream)
  (format stream "|"))

(defmethod make-binary-disjunction ((lhs formula) (rhs formula))
  (make-instance 'binary-disjunction
		 :lhs lhs
		 :rhs rhs))

(defgeneric make-multiple-arity-disjunction (&rest disjuncts))

(defmethod make-multiple-arity-disjunction (&rest disjuncts)
  (if disjuncts
      (if (cdr disjuncts)
	  (if (cddr disjuncts)
	      (make-instance 'multiple-arity-disjunction
			     :items disjuncts)
	      (make-instance 'binary-disjunction
			     :lhs (first disjuncts)
			     :rhs (second disjuncts)))
	  (first disjuncts))
      top))

(defun binary-disjunction->multiple-arity-disjunction (binary-disjunction)
  (make-instance 'multiple-arity-disjunction
		 :items (list (lhs binary-disjunction)
			      (rhs binary-disjunction))))

(defun multiple-arity-disjunction->binary-disjunction (multiple-arity-disjunction)
  (let ((disjuncts (items multiple-arity-disjunction)))
    (if (null disjuncts)
	(make-instance 'binary-disjunction
		       :lhs top
		       :rhs top)
	(if (null (cdr disjuncts))
	    (make-instance 'binary-disjunction
			   :lhs (first disjuncts)
			   :rhs contradiction)
	    (labels ((make-disjunction (ds)
		       (if (null (cddr ds))
			   (make-binary-disjunction (first ds)
						    (second ds))
			   (make-binary-disjunction (first ds)
						    (make-disjunction (cdr ds))))))
	      (make-disjunction disjuncts))))))

;; conjunctions

(defun binary-conjunction? (thing)
  (typep thing 'binary-conjunction))

(defmethod print-object ((con binary-conjunction) stream)
  (format stream "&"))

(defgeneric conjuncts (formula))

(defmethod conjuncts ((x t))
  (list x))

(defmethod conjuncts ((x binary-conjunction))
  (append (conjuncts (lhs x))
	  (conjuncts (rhs x))))

(defmethod conjuncts ((x null))
  nil)

(defmethod conjuncts ((l list))
  (reduce #'append (mapcar #'conjuncts l)))

(defclass multiple-arity-conjunction (multiple-arity-connective-formula)
  nil)

(defmethod conjuncts ((x multiple-arity-conjunction))
  (conjuncts (items x)))

(defun multiple-arity-conjunction? (thing)
  (eql (class-of thing) 'multiple-arity-conjunction))

(defmethod print-object ((mac multiple-arity-conjunction) stream)
  (format stream "&"))

(defun make-binary-conjunction (lhs rhs)
  (make-instance 'binary-conjunction
		 :lhs lhs
		 :rhs rhs))

(defun make-multiple-arity-conjunction (&rest conjuncts)
  (if conjuncts
      (if (cdr conjuncts)
	  (if (cddr conjuncts)
	      (make-instance 'multiple-arity-conjunction
			     :items conjuncts)
	      (make-instance 'binary-conjunction
			     :lhs (first conjuncts)
			     :rhs (second conjuncts)))
	  (first conjuncts))
      contradiction))

(defun binary-conjunction->multiple-arity-conjunction (binary-conjunction)
  (make-instance 'multiple-arity-conjunction
		 :items (list (lhs binary-conjunction)
			      (rhs binary-conjunction))))

(defun multiple-arity-conjunction->binary-conjunction (multiple-arity-conjunction)
  (let ((conjuncts (items multiple-arity-conjunction)))
    (if (null conjuncts)
	(make-binary-conjunction contradiction contradiction)
	(if (null (cdr conjuncts))
	    (make-instance 'binary-conjunction
			   :lhs (first conjuncts)
			   :rhs top)
	    (labels ((make-conjunction (ds)
		       (if (null (cddr ds))
			   (make-binary-conjunction (first ds)
						    (second ds))
			   (make-binary-conjunction (first ds)
						    (make-conjunction (cdr ds))))))
	      (make-conjunction conjuncts))))))

(defun universal-generalization? (thing)
  (eql (class-of thing) 'universal-generalization))

(defmethod print-object ((uni-gen universal-generalization) stream)
  (format stream "(! [~{~a~^,~}] : ~a)" (bindings uni-gen) (matrix uni-gen)))

(defun existential-generalization? (thing)
  (eql (class-of thing) 'existential-generalization))

(defmethod print-object ((exi-gen existential-generalization) stream)
  (format stream "(? [~{~a~^,~}] : ~a)" (bindings exi-gen) (matrix exi-gen)))

(defun make-universal (bindings formula)
  (make-instance 'universal-generalization
		 :bindings bindings
		 :matrix formula))

(defun make-existential (bindings matrix)
  (make-instance 'existential-generalization
		 :bindings bindings
		 :matrix matrix))

(defclass tptp-source ()
  nil)

(defclass inference-record (tptp-source)
  ((rule
    :accessor rule
    :initarg :rule
    :initform (error "An inference record needs a rule of inference"))
   (useful-info
    :type general-list
    :accessor useful-info
    :initarg :useful-info
    :initform (make-instance 'general-list))
   (parents
    :type general-list
    :accessor parents
    :initarg :parents
    :initform (make-instance 'general-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flatten
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric flatten-tptp (tptp-thing)
  (:documentation "All the atoms that can be reached from TPTP-THING."))

(defmethod flatten-tptp ((unhandled-tptp-thing t))
  (let ((class (class-of unhandled-tptp-thing)))
    (error "Don't know how to flatten TPTP things of class ~a." class)))

(defmethod flatten-tptp ((tptp-thing string))
  (list tptp-thing))

(defmethod flatten-tptp ((ir inference-record))
  (append (flatten-tptp (rule ir))
	  (flatten-tptp (useful-info ir))
	  (flatten-tptp (parents ir))))

(defmethod flatten-tptp ((tptp-thing cons))
  (append (flatten-tptp (car tptp-thing))
	  (flatten-tptp (cdr tptp-thing))))

(defmethod flatten-tptp ((tptp-thing null))
  nil)

(defmethod flatten-tptp ((tptp-thing integer))
  (list tptp-thing))

(defmethod flatten-tptp ((tptp-atom atomic-expression))
  (with-slots (head arguments)
      tptp-atom
    (if (and (string= (stringify head) "file")
	     (length= 2 arguments))
	(list tptp-atom) ;; this looks like an external source
	(apply #'append
	       (list (head tptp-atom))
	       (mapcar #'flatten-tptp (arguments tptp-atom))))))

(defmethod flatten-tptp ((tptp-atom atomic-formula))
  (apply #'append
	 (list (predicate tptp-atom))
	 (mapcar #'flatten-tptp (arguments tptp-atom))))

(defmethod flatten-tptp ((var variable-term))
  (list var))

(defmethod flatten-tptp ((l general-list))
  (flatten-tptp (terms l)))

(defgeneric negative-formula-p (formula))

(defmethod negative-formula-p ((formula t))
  nil)

(defmethod negative-formula-p ((neg negation))
  t)

(defmethod negative-formula-p ((eq disequation))
  t)

(defgeneric positivize (formula))

(defmethod positivize ((formula t))
  formula)

(defmethod positivize ((formula negation))
  (argument formula))

(defmethod positivize ((eq disequation))
  (make-instance 'equation
		 :lhs (lhs eq)
		 :rhs (rhs eq)))

(defun same-variable-name (variable-1 variable-2)
  (string= (stringify (head variable-1))
	   (stringify (head variable-2))))

(defgeneric free-variables (expression))

(defmethod free-variables :around ((expression t))
  (let ((free (call-next-method)))
    (remove-duplicates free :test #'same-variable-name :from-end t)))

(defmethod free-variables ((gen generalization))
  (with-slots (bindings matrix)
      gen
    (set-difference (free-variables matrix) bindings
		    :test #'same-variable-name)))

(defmethod free-variables ((x atomic-formula))
  (remove-if-not #'variable-term-p (flatten-tptp x)))

(defmethod free-variables ((x binary-connective-formula))
  (append (free-variables (lhs x))
	  (free-variables (rhs x))))

(defmethod free-variables ((x multiple-arity-connective-formula))
  (reduce #'append (mapcar #'free-variables (items x))))

(defmethod free-variables ((x negation))
  (free-variables (argument x)))

(defgeneric universally-close (x))

(defmethod universally-close ((formula formula))
  (let ((free (free-variables formula)))
    (if (null free)
	formula
	(make-instance 'universal-generalization
		       :bindings free
		       :matrix formula))))

(defmethod print-object ((ir inference-record) stream)
  (format stream "inference(~a,~a,~a)" (rule ir) (useful-info ir) (parents ir)))

(defgeneric flatten-conjunctions/disjunctions (tptp-thing))

(defmethod flatten-conjunctions/disjunctions ((x atomic-formula))
  x)

(defmethod flatten-conjunctions/disjunctions ((x multiple-arity-conjunction))
  (apply #'make-multiple-arity-conjunction
	 (mapcar #'flatten-conjunctions/disjunctions (conjuncts x))))

(defmethod flatten-conjunctions/disjunctions ((x multiple-arity-disjunction))
  (apply #'make-multiple-arity-disjunction
	 (mapcar #'flatten-conjunctions/disjunctions (disjuncts x))))

(defmethod flatten-conjunctions/disjunctions ((x binary-disjunction))
  (apply #'make-multiple-arity-disjunction
	 (mapcar #'flatten-conjunctions/disjunctions (disjuncts x))))

(defmethod flatten-conjunctions/disjunctions ((x binary-conjunction))
  (apply #'make-multiple-arity-conjunction
	 (mapcar #'flatten-conjunctions/disjunctions (conjuncts x))))

(defmethod flatten-conjunctions/disjunctions ((x negation))
  (make-instance 'negation
		 :argument (flatten-conjunctions/disjunctions (argument x))))

(defmethod flatten-conjunctions/disjunctions ((x binary-connective-formula))
  (make-instance (class-of x)
		 :lhs (flatten-conjunctions/disjunctions (lhs x))
		 :rhs (flatten-conjunctions/disjunctions (rhs x))))

(defmethod flatten-conjunctions/disjunctions ((gen generalization))
  (make-instance (class-of gen)
		 :bindings (bindings gen)
		 :matrix (flatten-conjunctions/disjunctions (matrix gen))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (Syntactic) equality of terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric equal-terms-p (term-1 term-2)
  (:documentation "Are terms TERM-1 and TERM-2 equal?"))

(defmethod equal-terms-p ((term-1 t) (term-2 t))
  nil)

(defmethod equal-terms-p ((term-1 atomic-expression) (term-2 atomic-expression))
  (let ((head-1 (head term-1))
	(head-2 (head term-2))
	(arguments-1 (arguments term-1))
	(arguments-2 (arguments term-2)))
    (when (string= (stringify head-1) (stringify head-2))
      (when (length= arguments-1 arguments-2)
	(loop
	   :for argument-1 :in arguments-1
	   :for argument-2 :in arguments-2
	   :unless (equal-terms-p argument-1 argument-2) :do (return nil)
	   :finally (return t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hunting for terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric terms-with-functor (functor-name tptp)
  (:documentation "The (function) terms inside TPTP whose functor is FUNCTOR-NAME."))

(defmethod terms-with-functor (functor-name (x null))
  nil)

(defmethod terms-with-functor (functor-name (l list))
  (let ((terms (mapcar #'(lambda (x) (terms-with-functor functor-name x)) l)))
    (remove-duplicates (reduce #'append terms) :test #'equal-terms-p)))

(defmethod terms-with-functor (functor-name (atom atomic-formula))
  (terms-with-functor functor-name (arguments atom)))

(defmethod terms-with-functor (functor-name (neg negation))
  (terms-with-functor functor-name (argument neg)))

(defmethod terms-with-functor (functor-name (x binary-connective-formula))
  (terms-with-functor functor-name (list (lhs x) (rhs x))))

(defmethod terms-with-functor (functor-name (x multiple-arity-connective-formula))
  (terms-with-functor functor-name (items x)))

(defmethod terms-with-functor (functor-name (gen generalization))
  (terms-with-functor functor-name (matrix gen)))

(defmethod terms-with-functor (functor-name (var variable-term))
  nil)

(defmethod terms-with-functor (functor-name (term atomic-expression))
  (if (string= (stringify functor-name)
	       (stringify (head term)))
      (cons term (terms-with-functor functor-name (arguments term)))
      (terms-with-functor functor-name (arguments term))))

(defgeneric contains-predicate? (expression predicate)
  (:documentation "Does EXPRESSION contain a subexpression with the predicate symbol PREDICATE?"))

(defmethod contains-predicate? ((atom atomic-expression) predicate)
  nil)

(defmethod contains-predicate? ((term function-term) predicate)
  nil)

(defmethod contains-predicate? ((term variable-term) predicate)
  nil)

(defmethod contains-predicate? ((atom atomic-formula) predicate)
  (string= (stringify (predicate atom)) (stringify predicate)))

(defmethod contains-predicate? ((neg negation) predicate)
  (contains-predicate? (argument neg) predicate))

(defmethod contains-predicate? ((multi multiple-arity-connective-formula) predicate)
  (some #'(lambda (x) (contains-predicate? x predicate))
	(items multi)))

(defmethod contains-predicate? ((x binary-connective-formula) predicate)
  (or (contains-predicate? (lhs x) predicate)
      (contains-predicate? (rhs x) predicate)))

(defmethod contains-predicate? ((gen generalization) predicate)
  (contains-predicate? (matrix gen) predicate))

;;; expressions.lisp ends here
