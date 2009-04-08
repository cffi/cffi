;; Load the libffi library
;; Liam Healy 2009-02-22 22:06:50EST library.lisp
;; Time-stamp: <2009-04-07 22:47:33EDT library.lisp>

(in-package :fsbv)

(cffi:load-foreign-library #+unix "libffi.so")
