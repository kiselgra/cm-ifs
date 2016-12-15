(cmu-c::cl-reader)

(defmacro for-generator (gen &body body) 
  `(case c-mera:*generator*
     (,gen ,@body)))

(require :c-mera)

(for-generator :c (require :cmu-c))
(for-generator (:cxx) (require :cmu-c++))
(for-generator (:cuda) (require :cmu-cuda))

(cmu-c::cm-reader)

(defmacro def-ifs-package-and-system ()
  `(progn
     (defpackage :cm-ifs
       ,(append (for-generator :c    '(:use :cmu-c))
		(for-generator :cxx  '(:use :cmu-cxx))
		(for-generator :cuda '(:use :cmu-cuda)))
       (:export :with-interface
		:*gen-interface*
		:*gen-dependencies*
		:interface-only
		:implementation-only))
     (asdf::defsystem cm-ifs
       :name "cm-ifs"
       :version "1.0.0"
       :serial t
       :components ((:file "src/interfaces"))
       :depends-on ,(append (for-generator :c    '("cmu-c"))
			    (for-generator :cxx  '("cmu-c++"))
			    (for-generator :cuda '("cmu-cuda"))))))

(def-ifs-package-and-system)
	 

