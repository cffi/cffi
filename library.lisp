;; Load the libffi library
;; Liam Healy 2009-02-22 22:06:50EST library.lisp
;; Time-stamp: <2009-02-22 22:07:22EST library.lisp>

(in-package :fsbv)

(load-foreign-library #+unix "libffi.so")
