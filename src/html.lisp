(in-package :cl-tptp)

(defgeneric render-html (tptp-thing session)
  (:documentation "An HTML rendering of TPTP-THING for the hunchentoot session SESSION."))

(defmethod render-html ((x tptp-formula) session)
  (with-slots (name role formula)
      x
    (let ((rendered-formula (render-html formula session)))
      (register-groups-bind (sans-outer-parens)
	  ("^[(](.+)[)]$" rendered-formula)
	(setf rendered-formula sans-outer-parens))
      (with-html-output-to-string (dummy)
	((:td :class "formula-name")
	 (fmt "~a" name))
	((:td :class (format nil "~a" role)))
	((:td :class "formula-proper")
	 (fmt "~a" rendered-formula))
	(if (slot-boundp x 'source)
	    (htm ((:td :class "formula-source")
		  (fmt "~a" (render-html (source x) session))))
	    (htm ((:td :class "formula-source"))))
	(if (slot-boundp x 'optional-info)
	    (htm ((:td :class "formula-optional-info")
		  (fmt "~a" (render-html (optional-info x) session))))
	    (htm ((:td :class "formula-optional-info"))))))))

(defmethod render-html ((fof fof) session)
  (with-html-output-to-string (dummy)
    ((:tr :id (format nil "~a" (name fof))
	  :class "fof")
     (fmt "~a" (call-next-method)))))

(defmethod render-html ((x cnf) session)
  (with-html-output-to-string (dummy)
    ((:tr :id (format nil "~a" (name x))
	  :class "cnf")
     (fmt "~a" (call-next-method)))))

(defmethod render-html ((formula-list null) session)
  "")

(defmethod render-html ((formula-list list) session)
  (with-html-output-to-string (dummy)
    ((:table :class "tptp-db" :title "TPTP formulas")
     (:caption "TPTP formulas")
     (:thead
      (:tr
       (:th "Name")
       (:th "Role")
       (:th "Formula")
       (:th "Source")
       (:th "Optional Info")))
     (:tbody
      (dolist (formula formula-list)
	(htm (fmt "~a" (render-html formula session)))))
     (:tfoot
      (:tr
       ((:td :colspan 3)
	(:p ((:span :class "conjecture") "Conjectures")
	    ", "
	    ((:span :class "definition") "Definitions")
	    ", "
	    ((:span :class "axiom") "Axioms")
	    ", "
	    ((:span :class "lemma") "Lemmas")
	    ", "
	    ((:span :class "hypothesis") "Hypotheses")
	    ", "
	    ((:span :class "plain") "Plain"))))))))

(defmethod render-html ((problem tptp-db) session)
  (with-slots (formulas)
      problem
    (with-html-output-to-string (dummy)
      (let ((includes (remove-if-not #'(lambda (x) (typep x 'include-instruction)) formulas))
	    (non-includes (remove-if #'(lambda (x) (typep x 'include-instruction)) formulas)))
	(when includes
	  (htm
	   ((:table :title "Include statements" :class "include-table")
	    (:caption "Include statements")
	    (:thead
	     (:tr
	      (:th "File")
	      (:th "Selection")))
	    (dolist (include includes)
	      (htm (:tbody
		    (fmt "~a" (render-html include session)))))
	    (:tfoot
	     ((:form :method "post"
		     :action "expand"
		     :enctype "multipart/form-data")
	      ((:input :type "submit"
		       :title "Expand these include statements"
		       :value "Expand")))))))
	(let ((session-problem (session-value :problem session))
	      (session-solution (session-value :solution session)))
	  (cond ((eq problem session-problem)
		 (htm (fmt "~a" (render-html non-includes session))))
		((eq problem session-solution)
		 (let ((solution-properties (session-value :solution-properties session)))
		   (let ((restrict-signature (gethash "restrict-signature" solution-properties))
			 (kowalski (gethash "kowalski" solution-properties))
			 (squeeze-quantifiers (gethash "squeeze-quantifiers" solution-properties))
			 (supporting-axioms (gethash "supporting-axioms" solution-properties))
			 (reduce-equivalences (gethash "reduce-equivalences" solution-properties)))
		     (let ((reformulated non-includes))
		       (when restrict-signature
			 (when (not (solution-restricted-p problem))
			   (setf reformulated (restrict-solution-to-problem-language problem))))
		       (when kowalski
			 (setf reformulated (kowalski reformulated)))
		       (when squeeze-quantifiers
			 (setf reformulated (squeeze-quantifiers reformulated)))
		       (when supporting-axioms
			 (setf reformulated (supporting-axioms (if (typep reformulated 'tptp-db)
								   reformulated
								   problem))))
		       (when reduce-equivalences
			 (setf reformulated (reduce-equivalences reformulated nil)))
		       (setf (session-value :solution session)
			     (if (typep reformulated 'tstp-db)
				 reformulated
				 (make-instance 'tstp-db
						:problem session-problem
						:restricted nil
						:formulas (if (typep reformulated 'list)
							      reformulated
							      (if (typep reformulated 'tptp-db)
								  (formulas reformulated)
								  (error "huh?"))))))
		       (htm (fmt "~a" (render-html reformulated session)))))))
		(t
		 (htm (fmt "~a" (render-html non-includes session))))))))))

(defmethod render-html ((include include-instruction) session)
  (with-slots (file selection)
      include
    (with-html-output-to-string (dummy)
      (:tr
       ((:td :class "file-name") (fmt "~a" file))
       (if (null selection)
	   (htm ((:td :class "formula-selection") "(none)"))
	   (htm ((:td :class "formula-selection")
		 (loop
		    :with len = (length selection)
		    :for formula :in selection
		    :for i :from 1
		    :do
		    (htm ((:span :class "formula-name") (fmt "~a" formula)))
		    (when (< i len)
		      (htm (str ", ")))))))))))

(defmethod render-html ((x atomic-expression) session)
  (with-slots (head arguments)
      x
    (if (null arguments)
	(with-html-output-to-string (dummy)
	  (htm ((:span :class "function-name") (fmt "~a" head))))
	(with-html-output-to-string (dummy)
	  (htm ((:span :class "function-name") (fmt "~a" head)))
	  (str "(")
	  (loop
	     :with len = (length arguments)
	     :for i :from 1 upto len
	     :for arg :in arguments
	     :do
	     (htm (fmt "~a" (render-html arg session)))
	     (when (< i len)
	       (htm (str ", "))))
	  (str ")")))))

(defmethod render-html ((l general-list) session)
  (with-slots (terms)
      l
    (with-html-output-to-string (dummy)
      (str "[")
      (loop
	 :with len = (length terms)
	 :for i :from 1 upto len
	 :for arg :in terms
	 :do
	 (htm (fmt "~a" (render-html arg session)))
	 (when (< i len)
	   (htm (str ", "))))
      (str "]"))))

(defmethod render-html ((x string) session)
  (multiple-value-bind (problem problem-set-p)
      (session-value :problem session)
    (multiple-value-bind (solution solution-set-p)
	(session-value :solution session)
      (if (or problem-set-p
	      solution-set-p)
	  (if (or (member x (when problem-set-p (formulas problem)) :test #'string= :key #'(lambda (x) (stringify (name x))))
		  (member x (when solution-set-p (formulas solution)) :test #'string= :key #'(lambda (x) (stringify (name x)))))
	      (with-html-output-to-string (dummy)
		((:a :href (format nil "#~a" x) :class "formula-reference") (fmt "~a" x)))
	      (with-html-output-to-string (dummy)
		(str "&ldquo;") (fmt "~a" x) (str "&rdquo;")))
	  (with-html-output-to-string (dummy)
	    (str "&ldquo;") (fmt "~a" x) (str "&rdquo;"))))))

(defmethod render-html ((x integer) session)
  (with-html-output-to-string (dummy)
    ((:a :href (format nil "#~d" x) :class "formula-reference") (fmt "~d" x))))

(defmethod render-html ((x function-term) session)
  (with-slots (head arguments)
      x
    (if (null arguments)
	(with-html-output-to-string (dummy)
	  (htm ((:span :class "function-name") (fmt "~a" head))))
	(with-html-output-to-string (dummy)
	  (htm ((:span :class "function-name") (fmt "~a" head)))
	  (str "(")
	  (loop
	     :with len = (length arguments)
	     :for i :from 1 upto len
	     :for arg :in arguments
	     :do
	     (htm (fmt "~a" (render-html arg session)))
	     (when (< i len)
	       (htm (str ", "))))
	  (str ")")))))

(defmethod render-html ((var variable-term) session)
  (with-slots (head)
      var
    (with-html-output-to-string (dummy)
      ((:span :class "variable") (fmt "~a" head)))))

(defmethod render-html ((x atomic-formula) session)
  (with-slots (predicate arguments)
      x
    (if (null arguments)
	(with-html-output-to-string (dummy)
	  (htm ((:span :class "predicate-symbol") (fmt "~a" predicate))))
	(with-html-output-to-string (dummy)
	  (htm ((:span :class "predicate-symbol") (fmt "~a" predicate)))
	  (str "(")
	  (loop
	     :with len = (length arguments)
	     :for i :from 1 upto len
	     :for arg :in arguments
	     :do
	     (htm (fmt "~a" (render-html arg session)))
	     (when (< i len)
	       (htm (str ", "))))
	  (str ")")))))

(defmethod render-html ((x (eql *nullary-true*)) session)
  (with-html-output-to-string (dummy)
    (str "&#8868;")))

(defmethod render-html ((x (eql *nullary-false*)) session)
  (with-html-output-to-string (dummy)
    (str "&#8869;")))

(defmethod render-html ((x equation) session)
  (with-slots (lhs rhs)
      x
    (with-html-output-to-string (dummy)
      (fmt "~a = ~a"
	   (render-html lhs session)
	   (render-html rhs session)))))

(defmethod render-html ((x disequation) session)
  (with-slots (lhs rhs)
      x
    (with-html-output-to-string (dummy)
      (fmt "~a &ne; ~a"
	   (render-html lhs session)
	   (render-html rhs session)))))

(defmethod render-html ((x negation) session)
  (with-html-output-to-string (dummy)
    (fmt "&not;~a" (render-html (argument x) session))))

(defmethod render-html ((dis binary-disjunction) session)
  (with-slots (lhs rhs)
      dis
    (with-html-output-to-string (dummy)
      (let ((disjuncts (disjuncts dis)))
	(loop
	   :with len = (length disjuncts)
	   :initially (htm (fmt "("))
	   :for i :from 1 :upto len
	   :for disjunct :in disjuncts
	   :do
	   (htm (fmt "~a" (render-html disjunct session)))
	   (when (< i len)
	     (htm (str " &or; ")))
	   :finally (htm (fmt ")")))))))

(defmethod render-html ((formula implication) session)
  (with-slots (lhs rhs)
      formula
    (with-html-output-to-string (dummy)
      (fmt "(~a &rarr; ~a)" (render-html lhs session) (render-html rhs session)))))

(defmethod render-html ((formula equivalence) session)
  (with-slots (lhs rhs)
      formula
    (with-html-output-to-string (dummy)
      (fmt "(~a &harr; ~a)" (render-html lhs session) (render-html rhs session)))))

(defmethod render-html ((formula universal-generalization) session)
  (with-slots (bindings matrix)
      formula
    (with-html-output-to-string (dummy)
      (str "&forall; [")
      (loop
	 :with len = (length bindings)
	 :for binding :in bindings
	 :for i :from 1 :upto len
	 :do
	 (htm ((:span :class "variable") (fmt "~a" binding)))
	 (when (< i len)
	   (htm (str ", "))))
      (fmt "] : ~a" (render-html matrix session)))))

(defmethod render-html ((formula existential-generalization) session)
  (with-slots (bindings matrix)
      formula
    (with-html-output-to-string (dummy)
      (str "&exist; [")
      (loop
	 :with len = (length bindings)
	 :for binding :in bindings
	 :for i :from 1 :upto len
	 :do
	 (htm ((:span :class "variable") (fmt "~a" binding)))
	 (when (< i len)
	   (htm (str ", "))))
      (fmt "] : ~a" (render-html matrix session)))))

(defmethod render-html ((dis multiple-arity-disjunction) session)
  (with-slots (items)
      dis
    (with-html-output-to-string (dummy)
      (let ((disjuncts (disjuncts items)))
	(loop
	   :with len = (length disjuncts)
	   :initially (htm (fmt "("))
	   :for i :from 1 :upto len
	   :for disjunct :in disjuncts
	   :do
	   (htm (fmt "~a" (render-html disjunct session)))
	   (when (< i len)
	     (htm (str " &or; ")))
	   :finally (htm (fmt ")")))))))

(defmethod render-html ((con binary-conjunction) session)
  (with-slots (lhs rhs)
      con
    (with-html-output-to-string (dummy)
      (let ((conjuncts (conjuncts con)))
	(loop
	   :with len = (length conjuncts)
	   :initially (htm (fmt "("))
	   :for i :from 1 :upto len
	   :for conjunct :in conjuncts
	   :do
	   (htm (fmt "~a" (render-html conjunct session)))
	   (when (< i len)
	     (htm (str " &and; ")))
	   :finally (htm (fmt ")")))))))

(defmethod render-html ((con multiple-arity-conjunction) session)
  (with-slots (items)
      con
    (with-html-output-to-string (dummy)
      (let ((conjuncts (conjuncts items)))
	(loop
	   :with len = (length conjuncts)
	   :initially (htm (fmt "("))
	   :for i :from 1 :upto len
	   :for conjunct :in conjuncts
	   :do
	   (htm (fmt "~a" (render-html conjunct session)))
	   (when (< i len)
	     (htm (str " &and; ")))
	   :finally (htm (fmt ")")))))))

(defmethod render-html ((ir inference-record) session)
  (with-slots (rule useful-info parents)
      ir
    (with-html-output-to-string (dummy)
      ((:span :class "tptp-keyword") "inference")
      (str "(")
      (fmt "~a,~a,~a" (render-html rule session) (render-html useful-info session) (render-html parents session))
      (str ")"))))
