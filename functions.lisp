;; Interface to libffi functions
;; Liam Healy 2009-04-06 21:14:55EDT functions.lisp
;; Time-stamp: <2009-04-07 22:18:04EDT functions.lisp>

(in-package :fsbv)

;;; See file:///usr/share/doc/libffi-dev/html/The-Basics.html#The-Basics

(cffi:defcfun ("ffi_prep_cif" prep-cif) status
    (ffi-cif :pointer)
    (ffi-abi abi)
    (nargs :uint)
    (rtype :pointer)
    (argtypes :pointer))

(cffi:defcfun ("ffi_call" call) :void
    (ffi-cif :pointer)
    (function :pointer)
    (rvalue :pointer)
    (avalues :pointer))

