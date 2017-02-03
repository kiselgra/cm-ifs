(in-package :cm-ifs)

;;; define interfaces and implementations at the same time

(defvar *gen-interface* nil) 
(defvar *gen-dependencies* nil) 

(defmacro for-generator (gen &body body) 
  `(case c-mera:*generator*
     (,gen ,@body)
     (otherwise `(,(gensym) () nil))))

(defmacro cpp-guard (name &body body)
  `(progn
     (comment ,name :prefix "#ifndef ")
     (comment ,name :prefix "#define ")
     ,@body
     (comment "" :prefix "")
     (comment ,name :prefix "#endif // ")
     ;; newline-at-eof
     (comment "" :prefix "")))

;;(format t "--> gen: ~a~%" c-mera:*generator*)

(defvar *search-path-for-use* (list "."))

(defun lookup-file (name suffix &key drop-suffix change-suffix)
  "Lookup a file in the directories listed in *SEARCH-PATH-FOR-USE*.
   This is a bit more complex as we have to probe lisp-files with their
   suffix, but have to return them without"
  (let ((full-name (concatenate 'string name "." suffix)))
    (loop for x in *search-path-for-use*
       with f
       do (setf f (concatenate 'string x "/" full-name))
       if (probe-file f)
       return (cl:cond (drop-suffix   (concatenate 'string x "/" name))
		       (change-suffix (concatenate 'string x "/" name "." change-suffix))
		       (t             f))
       finally (error "Cannot find module ~a (~a) in search path ~a." name suffix *search-path-for-use*))))

(defun use-with-dependencies (modules)
  (let ((lisp-files (mapcar (lambda (x) (lookup-file (format nil "~a" x) "lisp")) modules)))
    (format t "~a: ~{~a~^ ~}~%" *gen-dependencies* lisp-files)
    (let ((*gen-dependencies* nil)
	  (*gen-interface* t))
      (dolist (f lisp-files)
	(load f)))))

(defmacro with-interface ((name &key use include (load-fn #'cl:load)) &body body &environment env)
  (macrolet ((ignore-form (form) `'(,form (&body body) (declare (ignore body)) (values))))
    (cl:cond (*gen-dependencies*
	      (use-with-dependencies use))
	     (*gen-interface*
	      `(macrolet ((interface-only (&body body)
			    `(macrolet ((function (&body body) `(cms-c::function ,@body))
					,(for-generator (:cxx :cuda)
					   `(using-namespace (&body body) `(cms-c::using-namespace ,@body)))
					,(for-generator :c
					   `(decl (&body body) `(cms-c::decl ,@body)))
					,(for-generator (:cxx :cuda)
					   `(decl (&body body) `(cms-cxx::decl ,@body)))
					(struct (&body body) `(cms-c::struct ,@body))
					;; ... all those handled below
					)
			       (progn ,@body)))
			  (function (name args arrow ret &body body)
			     (declare (ignore body))
			     `(cms-c::function ,name ,args ,arrow ,ret))
			  ,(for-generator :c
			     `(cmu-c::decl (decls &body body)
				`(cms-c::decl ,(loop for item in decls collect
						    (multiple-value-bind (spec type name) (cm-c:decompose-declaration item)
						      `(extern ,@spec ,type ,name)))
				   ,@body)))
			  ,(for-generator (:cxx :cuda)
			     `(cmu-cxx::decl (decls &body body)
				`(cms-cxx::decl ,(loop for item in decls collect
						      (multiple-value-bind (spec type name) (cm-cxx:decompose-declaration item)
							`(extern ,@spec ,type ,name)))
				   ,@body)))
			  ,(for-generator (:cxx :cuda) (ignore-form using-namespace))
			  (struct (&body body)
			    `(interface-only (struct ,@body)))
			  (class (&body body)
			      ;;(macrolet ((revert-from (f pack) `(,f (&body body) `(,',(intern f pack) ,@body))))
			      `(macrolet ((function (&rest rest) `(cms-c::function ,@rest))
					  (struct   (&rest rest) `(cms-c::struct ,@rest))
					  (class    (&rest rest) `(cms-cxx::class ,@rest))
					  (decl     (&rest rest) `(cms-cxx::decl ,@rest))
					  (using-namespace (&rest rest) `(cms-cxx::using-namespace ,@rest)))
				 (class ,@body)))
			  ,(ignore-form implementation-only)
			  ;; ...
			  )
		 (cpp-guard ,(substitute-if #\_ (lambda (c) (case c 
							      ((#\-) t) 
							      (otherwise nil)))
					    (string-upcase (format nil "__~a_h__" name)))
		   (comment "INTERFACE GENERATED WITH CM-IFS")
		   ,@(loop for x in use collect
			  `(progn (cl:funcall ,load-fn ,(lookup-file (format nil "~a" x) "lisp" :drop-suffix t))
				  (include ,(lookup-file (format nil "~a" x) "lisp" :change-suffix "h"))))
		   ,@(loop for x in include collect `(include ,(format nil "~a.h" x)))
		   ,@body)))
	     (t
	      `(macrolet ((implementation-only (&body body)
			    `(macrolet ((include (&body body) `(cms-c::include ,@body))
					(struct (&body body) `(cms-c::struct ,@body))
					,(for-generator (:cxx :cuda)
					   `(class (&body body) `(cms-cxx::class ,@body)))
					;; ... all those handled below
					)
			       (progn ,@body)))
			  ,(ignore-form interface-only)
			  ,(ignore-form include)
			  ,(ignore-form struct)
			  ,(for-generator (:cxx :cuda) (ignore-form class))
			  ;; ...
			  )
		 (progn
		   (comment "IMPLEMENTATION GENERATED WITH CM-IFS")
		   ,@(loop for x in use collect
			  `(progn (comment ,(format nil "sourcing ~a..." (lookup-file (format nil "~a" x) "lisp" :drop-suffix t)))
				  (cl:funcall ,load-fn ,(lookup-file (format nil "~a" x) "lisp" :drop-suffix t)))
			  )
		   (implementation-only (include ,(format nil "~a.h" name)))
		   ,@body))))))
 

(defmacro implementation-only (&body body)
  (cl:if *gen-interface*
	 (values)
	 `(progn ,@body)))

(defmacro header (name)
  `(implementation-only (include ,(format nil "~a.h" name))))


(defmacro use (&rest modules)
  (cl:if *gen-dependencies*
         (cl:progn
	      ;(format t "~a: ~{~a~^ ~}~%" *gen-dependencies* (mapcar (lambda (x) (lookup-file (format nil "~a" x) "lisp")) modules))
	   (use-with-dependencies modules)
         (values))
  `(progn ,@(loop for m in modules append
		 `((let ((*gen-interface* t))
		     (load ,(lookup-file (format nil "~a" m) "lisp"))
		     (values))
		   (include ,(lookup-file (format nil "~a" m) "h")))))))

