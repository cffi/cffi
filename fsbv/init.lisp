;; Load foreign library
;; Liam Healy 2009-02-22 09:55:45EST pkgdcl.lisp
;; Time-stamp: <2010-11-30 12:26:11EST init.lisp>

(in-package :common-lisp-user)

(cffi:load-foreign-library
 #+darwin "libffi.dylib"
 #+(and (not darwin) unix) "libffi.so")

(pushnew :fsbv *features*)
