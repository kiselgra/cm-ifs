;(require :cm-ifs)
;(use-package :cm-ifs)

;; how to mix c/c++/etc?

;(with-interface (test :use (indirect))
  (function test ((int argc) (char **argv)) -> (unsigned int)
    (for ((int i = 0) (< i argc) ++i)
      (printf "-> %s\\n" argv[i])))
;  )
