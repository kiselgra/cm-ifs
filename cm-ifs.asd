(cmu-cxx::cl-reader)
(require :c-mera)
(require :cmu-c++)
(cmu-cxx::cm-reader)

(defpackage :cm-ifs
  (:use :cmu-cxx)
  (:export :with-interface
	:*gen-interface*
	:*gen-dependencies*
	   ))
	 

(asdf::defsystem cm-ifs
    :name "cm-ifs"
    :version "1.0.0"
    :serial t
    :components ((:file "src/interfaces"))
    :depends-on ("cmu-c++"))
