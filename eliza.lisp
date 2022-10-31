(defconstant fail nil)

(defconstant no-bindings '((T . T)))

(defun eliza ()
  (loop
    (print 'eliza>)
    (write (flatten (use-eliza-rules (read))) :pretty t)))

(defun use-eliza-rules (input)
  (some #'(lambda (rule)
	    (let ((result (pat-match (rule-pattern rule) input)))
	      (if (not (eq result fail))
		  (sublis (switch-viewpoint result)
			  (random-elt (rule-responses rule))))))
	*eliza-rules*))

(defun flatten (input &optional accumulator)
  (cond ((null input) accumulator)
	((atom input) (cons input accumulator))
	(t (flatten (first input)
		    (flatten (rest input) accumulator)))))

(defun simple-equal (x y)
  (if (or (atom x) (atom y))
      (eql x y)
      (and (simple-equal (first x) (first y))
	   (simple-equal (rest x) (rest y)))))

(defun pat-match (pattern input &optional (bindings no-bindings))
				  (cond ((eq bindings fail) fail)
					((variable-p pattern)
					 (match-variable pattern input bindings))
					((eql pattern input) bindings)
					((and (consp pattern) (consp input))
					 (pat-match (rest pattern) (rest input) (pat-match (first pattern) (first input) bindings)))
					(t fail)))

(defun match-variable (var input bindings)
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
	  ((equal input (binding-val binding)) bindings)
	  (t fail))))

(defun variable-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0)#\?)))

(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-val (binding)
  (cdr binding))

(defun lookup (var bindings)
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  (cons (cons var val)
	(if (eq bindings no-bindings)
	    nil bindings)))
