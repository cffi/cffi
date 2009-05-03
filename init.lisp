;; Declaration of package
;; Liam Healy 2009-02-22 09:55:45EST pkgdcl.lisp
;; Time-stamp: <2009-05-03 09:27:13EDT init.lisp>

(in-package :common-lisp-user)

(defpackage :foreign-structures-by-value
  (:nicknames #:fsbv)
  (:use #:common-lisp))

(cffi:load-foreign-library #+unix "libffi.so")

(pushnew :fsbv *features*)
