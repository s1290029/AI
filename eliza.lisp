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

(defun rule-pattern (rule)
  (first rule))

(defun rule-responses (rule)
  (rest rule))

(defparameter *eliza-rules*
  '((((?* ?x) hello (?* ?y))
     (How do you do. Please state your problem.))
    (((?* ?x) I want (?* ?y))
     (What would it mean if you got ?y)
     (Why do you want ?y) (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y))
     (Do you really think its likely that ?y) (Do you wish that ?y)
     (What do you think about ?y) (Really-- if ?y))
    (((?* ?x) no (?* ?y))
     (Why not?) (You are being a bit negative)
     (Are you saying "NO" just to be negative?))
    (((?* ?x) I was (?* ?y))
     (Were you really?) (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))
    (((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?y))
     (What other feelings do you have?))))

(defun switch-viewpoint (words)
  (sublis '((I . you) (you . I) (me . you) (am . are)) words))

(defun flatten (the-list)
  (mappend #'mklist the-list))

(defun mklist (x)
  (if (listp x) x
      (list x)))

(defun mappend (fn the-list)
  (apply #'append (mapcar fn the-list)))

(defun random-elt (choices)
  (elt choices (random (length choices))))

(defun starts-with (list x)
  (and (consp list) (eql (first list) x)))

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
					((segment-pattern-p pattern)
					 (segment-match pattern input bindings))
					((and (consp pattern) (consp input))
					 (pat-match (rest pattern) (rest input) (pat-match (first pattern) (first input) bindings)))
					(t fail)))

(defun segment-match (pattern input bindings &optional (start 0))
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (if (null pat)
	(match-variable var input bindings)
	(let ((pos (position (first pat) input
			     :start start
			     :test #'equal)))
	  (if (null pos)
	      fail (let ((b2 (pat-match pat (subseq input pos)
					(match-variable var (subseq input 0 pos) bindings))))	  
			 (if (eq b2 fail)
			     (segment-match pattern input bindings (+ pos 1)) b2)))))))

(defun segment-pattern-p (pattern)
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

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
