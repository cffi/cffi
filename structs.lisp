;; C struct definitions
;; Liam Healy 2009-02-22 21:55:14EST structs.lisp
;; Time-stamp: <2009-02-22 21:55:37EST structs.lisp>

(in-package :fsbv)

(cffi:defcstruct ffi-cif
  (abi ffi-abi)
  (number-of-arguments unsigned)
  (argument-types :pointer)
  (return-type :pointer)
  (bytes unsigned)
  (flags unsigned))
