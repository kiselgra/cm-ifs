all: test.h test.cpp test.d test

test.h: test.lisp Makefile ../src/interfaces.lisp
	cm c test.lisp -E '(require :cm-ifs)' -E '(setf cm-ifs:*gen-interface* t)' | tee $@

test.cpp: test.lisp Makefile ../src/interfaces.lisp
	cm c test.lisp -E '(require :cm-ifs)' -E '(setf cm-ifs:*gen-interface* nil)' | tee $@

test.d: test.lisp Makefile ../src/interfaces.lisp
	cm c test.lisp -E '(require :cm-ifs)' -E '(setf cm-ifs:*gen-dependencies* "test.h")' | tee $@
	
test: test.cpp test.h
	gcc $< -o $@

clean:
	rm -f test.h test.cpp test.d
