;; libffi types
;; Liam Healy 2009-02-22 19:35:20EST types.lisp
;; Time-stamp: <2009-05-02 14:00:43EDT defs.lisp>

(in-package :fsbv)

;;; Types

(cffi:defcvar ("ffi_type_double" +size-double+ :read-only t) :int)
(cffi:defcvar ("ffi_type_float" +size-float+ :read-only t) :int)
(cffi:defcvar ("ffi_type_longdouble" +size-longdouble+ :read-only t) :int)
(cffi:defcvar ("ffi_type_pointer" +size-pointer+ :read-only t) :int)
(cffi:defcvar ("ffi_type_sint16" +size-sint16+ :read-only t) :int)
(cffi:defcvar ("ffi_type_sint32" +size-sint32+ :read-only t) :int)
(cffi:defcvar ("ffi_type_sint64" +size-sint64+ :read-only t) :int)
(cffi:defcvar ("ffi_type_sint8" +size-sint8+ :read-only t) :int)
(cffi:defcvar ("ffi_type_uint16" +size-uint16+ :read-only t) :int)
(cffi:defcvar ("ffi_type_uint32" +size-uint32+ :read-only t) :int)
(cffi:defcvar ("ffi_type_uint64" +size-uint64+ :read-only t) :int)
(cffi:defcvar ("ffi_type_uint8" +size-uint8+ :read-only t) :int)
(cffi:defcvar ("ffi_type_void" +size-void+ :read-only t) :int)

(defmacro libffi-type-pointer (symbol)
  "Get the pointer into the libffi library that represents the type
   for the given symbol."
  `(get ',symbol 'type-pointer))

;;; Structs

(cffi:defcstruct ffi-cif
  (abi ffi-abi)
  (number-of-arguments unsigned)
  (argument-types :pointer)
  (return-type :pointer)
  (bytes unsigned)
  (flags unsigned))

;;; Functions
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
