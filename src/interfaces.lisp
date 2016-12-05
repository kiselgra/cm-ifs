;;; this is the meh-eh-eh-ta file.

;;; basic package setup

(in-package :cm-ifs)

;;; define interfaces and implementations at the same time

(defvar *gen-interface* nil) 
(defvar *gen-dependencies* nil) 
(defmacro block-comment (text &body body)
  `(progn (comment ,text :prefix "/* ")
	  ,@body
	  (comment "" :prefix " */")))

(defun tonly (x) (cl:if x t nil))

(defmacro expand-1 (form &environment env)
  (multiple-value-bind (expansion expanded-p)
      (macroexpand-1 form env)
    `(values ',expansion ',expanded-p)))

;; for macrop see c-mera/src/core/utils.lisp
(defmacro indirect-macrop (form env)
  `(multiple-value-bind (e p) (macroexpand-1 ,form ,env) (values p e))) ; values is just to muffle the 'defined but not used' warning

(defparameter *possible-debug-values* '(user-macro))
(defparameter *debug* '(user-macro))

(lisp
 (defun string-append (string-1 string-2 &rest rest)
   (let ((str (concatenate 'string string-1 string-2)))
     (if rest
 	 (apply #'string-append (cons str (cons (car rest) (cdr rest))))
 	 str)))

 (defun debug-out (format class &rest format-args)
   (if (find class *debug*)
       (apply #'format t (string-append "## " format) format-args))))

(defun cmu-name (name)
  (let ((p (symbol-package name)))
    (debug-out 'user-macro "sympack of ~a is ~a.~%" name p)
    (let ((res (cl:or (eql p (find-package :cmu-c))
                      (eql p (find-package :cmu-cxx))
                      (eql p (find-package :cmu-cuda))
                      ;; ...
                      )))
      (debug-out 'user-macro "thus -> ~a~%" res)
      res)))

(defmacro with-interface ((name &key use) &body body &environment env)
  (labels ((user-macro (form)
	     (debug-out 'user-macro "--> ~a [ ~a / ~a / ~a ]~%" form
			(cl:not (cmu-name (first form)))
			(indirect-macrop form env)
			(tonly (cl:not (cl:or (eql (first form) 'interface-only)
					      (eql (first form) 'implementation-only)))))
	     (cl:and (cl:not (cmu-name (first form))) ;(eql (symbol-package (first form)) (find-package :cmu-cxx)))
		     (indirect-macrop form env)
		     (cl:not (cl:or (eql (first form) 'interface-only)
				    (eql (first form) 'implementation-only)))))
	   (interface-version (form)
	     ;;(format t "--> IF ~a~%" form)
	     (cl:if (user-macro form)
		    (interface-version (macroexpand-1 form env))
		    (case (first form)
		      ;; generate declaration from function
		      (function (destructuring-bind (tag name params _ ret &body body) form
				  (declare (ignore _ body))
				  `(progn (,tag ,name ,params -> ,ret))))
		      ;; forms can be forced to go in the interface
		      (interface-only form)
		      ;; keep those forms
		      ((include class struct typedef use-variables comment defmacro cl:progn) form)
		      ;; throw out these forms
		      ((implementation-only using-namespace) (values))
		      ;; descend into progns
		      (progn (cons 'progn (loop for x in (rest form) collect (interface-version x))))
		      ;; similarly for decls, but also change to 'extern' and remove initialization
		      (decl `(decl ,(loop for x in (second form)
					 if (find '= x) collect
					     `(extern ,@(butlast (butlast x)))
					 else collect
					     `(extern ,@x))
			       ,@(loop for x in (rest (rest form)) collect (interface-version x))))
		      ;; swallow whatever we don't know how to handle
		      (otherwise  `(block-comment "skipping this form as I don't know how to handle it:" (progn (comment (format nil "--> ~a~%" ',form) :prefix "   ") ,form))))))
	   (plain-version (form)
	     ;;(format t "--> PL ~a~%" form)
	     (cl:if (user-macro form)
		    (plain-version (macroexpand-1 form env))
		    (case (first form)
		      ;; things we keep in the implementation
		      (function form)
		      ((using-namespace) form)
		      ;; forms can be forced to go in the implementation
		      (implementation-only form)
		      ;; keep those forms
		      ((use-variables) form)
		      ;; things we throw out
		      ((include class interface-only defmacro) (values))
		      ;; descend into progns
		      (progn (cons 'progn (loop for x in (rest form) collect (plain-version x))))
		      ;; similarly for decls
		      (decl `(decl ,(second form)
			       ,@(loop for x in (rest (rest form)) collect (plain-version x))))
		      ;; swallow whatever we don't know how to handle
		      (otherwise  `(block-comment "skipping this form as I don't know how to handle it:" (progn (comment (format nil "--> ~a~%" ',form) :prefix "   ") ,form)))))))
    (cl:if (cl:not *gen-dependencies*)
	   (loop for x in use do (load (format nil "~a.lisp" x))))
    (cl:if *gen-dependencies*
	   (progn
	     (format t "~a: ~{~a.lisp~^ ~}~%" *gen-dependencies* use))   ;; we need this to rebuild a cpp/h file when a lisp-file loaded by its base lisp file is changed.
	     (cl:if *gen-interface*
		    `(progn
					;(substitute-if #\_ #'upper-case-p "Groucho Marx")
		       (cpp-guard ,(substitute-if #\_ (lambda (c) (case c 
								    ((#\-) t) 
								    (otherwise nil)))
						  (string-upcase (format nil "__~a_h__" name)))
			 ,@(loop for x in use collect `(include ,(format nil "~a.h" x)))
			 ,@(loop for form in body collect (interface-version form))))
		    `(progn 
		       (include ,(format nil "~a.h" name))
		       ,@(loop for x in use collect `(include ,(format nil "~a.h" x)))
		       ,@(loop for form in body collect (plain-version form)))))))


(defmacro implementation-only (&body body)
  (cl:if *gen-interface*
	 (values)
	 `(progn ,@body)))

(defmacro header (name)
  `(implementation-only (include ,(format nil "~a.h" name))))

(defmacro use (&rest modules)
  `(progn ,@(loop for m in modules append
		 `((let ((*gen-interface* t))
		     (load ,(format nil "~a.lisp" m))
		     (values))
		   (include ,(format nil "~a.h" m))))))

