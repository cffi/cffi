;; libffi types
;; Liam Healy 2009-02-22 19:35:20EST types.lisp
;; Time-stamp: <2009-04-06 22:37:04EDT types.lisp>

(in-package :fsbv)

(defcvar ("ffi_type_double" +size-double+ :read-only t) :int)
(defcvar ("ffi_type_float" +size-float+ :read-only t) :int)
(defcvar ("ffi_type_longdouble" +size-longdouble+ :read-only t) :int)
(defcvar ("ffi_type_pointer" +size-pointer+ :read-only t) :int)
(defcvar ("ffi_type_sint16" +size-sint16+ :read-only t) :int)
(defcvar ("ffi_type_sint32" +size-sint32+ :read-only t) :int)
(defcvar ("ffi_type_sint64" +size-sint64+ :read-only t) :int)
(defcvar ("ffi_type_sint8" +size-sint8+ :read-only t) :int)
(defcvar ("ffi_type_uint16" +size-uint16+ :read-only t) :int)
(defcvar ("ffi_type_uint32" +size-uint32+ :read-only t) :int)
(defcvar ("ffi_type_uint64" +size-uint64+ :read-only t) :int)
(defcvar ("ffi_type_uint8" +size-uint8+ :read-only t) :int)
(defcvar ("ffi_type_void" +size-void+ :read-only t) :int)

(defvar +pointer-type-double+ (foreign-symbol-pointer "ffi_type_double"))
(defvar +pointer-type-float+ (foreign-symbol-pointer "ffi_type_float"))
(defvar +pointer-type-longdouble+ (foreign-symbol-pointer "ffi_type_longdouble"))
(defvar +pointer-type-pointer+ (foreign-symbol-pointer "ffi_type_pointer"))
(defvar +pointer-type-sint16+ (foreign-symbol-pointer "ffi_type_sint16"))
(defvar +pointer-type-sint32+ (foreign-symbol-pointer "ffi_type_sint32"))
(defvar +pointer-type-sint64+ (foreign-symbol-pointer "ffi_type_sint64"))
(defvar +pointer-type-sint8+ (foreign-symbol-pointer "ffi_type_sint8"))
(defvar +pointer-type-uint16+ (foreign-symbol-pointer "ffi_type_uint16"))
(defvar +pointer-type-uint32+ (foreign-symbol-pointer "ffi_type_uint32"))
(defvar +pointer-type-uint64+ (foreign-symbol-pointer "ffi_type_uint64"))
(defvar +pointer-type-uint8+ (foreign-symbol-pointer "ffi_type_uint8"))
