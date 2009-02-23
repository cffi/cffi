;; CFFI-Grovel definitions for unix systems.
;; Liam Healy 2009-02-22 09:24:33EST libffi-unix.lisp
;; Time-stamp: <2009-02-22 22:20:32EST libffi-unix.lisp>
;; $Id: $

#+linux
(define "_GNU_SOURCE")

(in-package :fsbv)

(include "ffi.h")

(cenum status
 ((:OK "FFI_OK"))
 ((:bad-typedef "FFI_BAD_TYPEDEF"))
 ((:bad-abi "FFI_BAD_ABI")))

(cenum abi
 ((:default-abi "FFI_DEFAULT_ABI"))
 ((:sysv "FFI_SYSV"))
 ((:unix64 "FFI_UNIX64")))

(ctype ffi-abi "ffi_abi")

(ctype sizet "size_t")
(ctype ushort "unsigned short")
(ctype unsigned "unsigned")

(cstruct ffi-type "struct _ffi_type"
  (size    "size"     :type sizet)
  (alignment "alignment"     :type ushort)
  (type    "type"     :type ushort)
  (elements   "elements"   :type :pointer))

#|
;;; Will not compile
;;; error: invalid application of ‘sizeof’ to incomplete type ‘struct ffi_cif’ 
;;; When structs are defined with the name at the end, apparently they
;;; are intended to be "opaque types".
(cstruct ffi-cif "struct ffi_cif"
 (abi    "abi"     :type ffi-abi)
 (nargs "nargs"     :type unsigned)
 (arg-types    "arg_types"     :type :pointer)
 (return-type   "rtype"   :type :pointer)
 (bytes   "bytes"   :type :unsigned)
 (flags   "flags"   :type :unsigned))
|#
