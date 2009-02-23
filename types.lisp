;; libffi types
;; Liam Healy 2009-02-22 19:35:20EST types.lisp
;; Time-stamp: <2009-02-22 22:13:34EST types.lisp>

(in-package :fsbv)

(defcvar ("ffi_type_double" +type-double+ :read-only t) :pointer)
(defcvar ("ffi_type_float" +type-float+ :read-only t) :pointer)
(defcvar ("ffi_type_longdouble" +type-longdouble+ :read-only t) :pointer)
(defcvar ("ffi_type_pointer" +type-pointer+ :read-only t) :pointer)
(defcvar ("ffi_type_sint16" +type-sint16+ :read-only t) :pointer)
(defcvar ("ffi_type_sint32" +type-sint32+ :read-only t) :pointer)
(defcvar ("ffi_type_sint64" +type-sint64+ :read-only t) :pointer)
(defcvar ("ffi_type_sint8" +type-sint8+ :read-only t) :pointer)
(defcvar ("ffi_type_uint16" +type-uint16+ :read-only t) :pointer)
(defcvar ("ffi_type_uint32" +type-uint32+ :read-only t) :pointer)
(defcvar ("ffi_type_uint64" +type-uint64+ :read-only t) :pointer)
(defcvar ("ffi_type_uint8" +type-uint8+ :read-only t) :pointer)
(defcvar ("ffi_type_void" +type-void+ :read-only t) :pointer)
