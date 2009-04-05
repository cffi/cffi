;; libffi types
;; Liam Healy 2009-02-22 19:35:20EST types.lisp
;; Time-stamp: <2009-02-23 08:39:07EST types.lisp>

(in-package :fsbv)

(defcvar ("ffi_type_double" +type-double+ :read-only t) :int)
(defcvar ("ffi_type_float" +type-float+ :read-only t) :int)
(defcvar ("ffi_type_longdouble" +type-longdouble+ :read-only t) :int)
(defcvar ("ffi_type_pointer" +type-pointer+ :read-only t) :int)
(defcvar ("ffi_type_sint16" +type-sint16+ :read-only t) :int)
(defcvar ("ffi_type_sint32" +type-sint32+ :read-only t) :int)
(defcvar ("ffi_type_sint64" +type-sint64+ :read-only t) :int)
(defcvar ("ffi_type_sint8" +type-sint8+ :read-only t) :int)
(defcvar ("ffi_type_uint16" +type-uint16+ :read-only t) :int)
(defcvar ("ffi_type_uint32" +type-uint32+ :read-only t) :int)
(defcvar ("ffi_type_uint64" +type-uint64+ :read-only t) :int)
(defcvar ("ffi_type_uint8" +type-uint8+ :read-only t) :int)
(defcvar ("ffi_type_void" +type-void+ :read-only t) :int)

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
