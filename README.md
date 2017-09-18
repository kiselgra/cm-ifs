An interface library for C-Mera
===============================

This is a small library that helps you not repeat yourself by generating C-style header and source files from a single C-Mera file. All sub-forms of `WITH-INTERFACE` are wrapped such that the proper declarations/definitions go in the appropriate files.

Note: The list of wrapper-macros is probably neither complete, nor (for C++) correct for all cases. We are happy about bug-reports and even more so patches :)


Setup
-----
Very simple :)

`ln -s <path-to-cm-ifs> ~/quicklisp/local-projects/cm-ifs`

Add
   (with-interface 1)
to your `cm.indent` file when using cm-mode in Emacs.


Examples
--------
See test/Makefile and test/test.lisp


Use
---

`(WITH-INTERFACE name &key use include load-fn &body body)`

You should only have one of those per file.

`name`
> Name of the interface.
> The include statement in the generated C++-File will refer to this name.

`use`
> List of names of modules to use, i.e. to LOAD (using load-fn).
> This increases build times (esp. with recursive use), but forwards macro definitions et al.

`include`
> List of names of modules to only emit include statements for.

`load-fn`
> Function used to load files (defaulting to CL:LOAD).
> Can be used to search for files in more complex directory settings.


### Subforms

`(interface-only ...)`
> Embedded code will be emitted to the header file, only.

`(implementation-only ...)`
> Embedded code will be emitted to the source file, only.
