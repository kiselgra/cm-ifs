;(require :cm-ifs)
(use-package :cm-ifs)

(defmacro test-ifs (spec)
  `(progn
     (format t "--> ~a~%------------~%~%" (macroexpand-1 ',spec))
     ,spec))
  

(test-ifs
 (with-interface (test :use (indirect))
   (include <vector>)
   (implementation-only
    (include <iostream>))
   (using-namespace std)
   (function test ((int argc) (char **argv)) -> (unsigned int)
     (for ((int i = 0) (< i argc) ++i)
       (printf "-> %s\\n" argv[i])))
   (decl ((const unsigned int foo = 123)
	  (int *p = 0)
	  ;(int* q[10] { 1 2 3 }))
	  )
     )
   (interface-only
    (function test2 ((int argc) (char **argv)) -> (unsigned int)
      (for ((int i = 0) (< i argc) ++i)
   	(printf "-> %s\\n" argv[i])))
    )
))
