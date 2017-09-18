(use-package :cm-ifs)

(with-interface (test)

  (struct X
    (decl ((int i))))
  (typedef struct X X)

  (function foo-1 ((int i)) -> X
    (return (cast X (clist i))))
  
  (function foo-2 ((X x)) -> int
    (return x.i))

  (implementation-only
   (include <stdio.h>)
	    
   (function main () -> int
     (return (foo-2 (foo-1 123))))))
