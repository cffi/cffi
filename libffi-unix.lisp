;; CFFI-Grovel definitions for unix systems.
;; Liam Healy 2009-02-22 09:24:33EST libffi-unix.lisp
;; Time-stamp: <2009-02-22 09:45:17EST libffi-unix.lisp>
;; $Id: $

(in-package :fsbv)

(include "ffi.h")

(constant (ffi-default-abi "FFI_DEFAULT_ABI"))
(constant (ffi-ok "FFI_OK"))

(ctype cif "ffi_cif")
(ctype type-void "ffi_type_void")
(ctype type-uint8 "ffi_type_uint8")
(ctype type-sint8 "ffi_type_sint8")
(ctype type-uchar "ffi_type_uchar")
(ctype type-schar "ffi_type_schar")
(ctype type-uint16 "ffi_type_uint16")
(ctype type-sint16 "ffi_type_sint16")
(ctype type-ushort "ffi_type_ushort")
(ctype type-sshort "ffi_type_sshort")
(ctype type-uint32 "ffi_type_uint32")
(ctype type-sint32 "ffi_type_sint32")
(ctype type-uint "ffi_type_uint")
(ctype type-sint "ffi_type_sint")
(ctype type-uint64 "ffi_type_uint64")
(ctype type-sint64 "ffi_type_sint64")
(ctype type-ulong "ffi_type_ulong")
(ctype type-slong "ffi_type_slong")
(ctype type-float "ffi_type_float")
(ctype type-double "ffi_type_double")
(ctype type-longdouble "ffi_type_longdouble")
(ctype type-pointer "ffi_type_pointer")
