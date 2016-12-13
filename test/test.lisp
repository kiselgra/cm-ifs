;(require :cm-ifs)
(use-package :cm-ifs)

;; how to mix c/c++/etc?
(format t "--> ~a~%" (macroexpand-1 '(with-interface (test :use (indirect))
  (implementation-only
    (include <iostream>))
  (using-namespace std)
  (function test ((int argc) (char **argv)) -> (unsigned int)
    (for ((int i = 0) (< i argc) ++i)
      (printf "-> %s\\n" argv[i])))
  (interface-only
    (function test2 ((int argc) (char **argv)) -> (unsigned int)
      (for ((int i = 0) (< i argc) ++i)
        (printf "-> %s\\n" argv[i])))
  ))))

(with-interface (test :use (indirect))
  (include <vector>)
  (implementation-only
    (include <iostream>))
  (using-namespace std)
  (function test ((int argc) (char **argv)) -> (unsigned int)
    (for ((int i = 0) (< i argc) ++i)
      (printf "-> %s\\n" argv[i])))
  (interface-only
    (function test2 ((int argc) (char **argv)) -> (unsigned int)
      (for ((int i = 0) (< i argc) ++i)
        (printf "-> %s\\n" argv[i])))
  ))
