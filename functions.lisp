;; Interface to libffi functions
;; Liam Healy 2009-04-06 21:14:55EDT functions.lisp
;; Time-stamp: <2009-04-06 23:43:46EDT functions.lisp>

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

;;(load-foreign-library #+unix "libgslcblas.so")
;;(load-foreign-library #+unix "libgsl.so")

;; (cffi:foreign-funcall "gsl_complex_abs" ???? #C(1.0d0 2.0d0) :double)

(defvar +pointer-type-complex+
  ;; See file:///usr/share/doc/libffi-dev/html/Structures.html#Structures
  (let ((ptr (cffi:foreign-alloc 'ffi-type))
	(complex (cffi:foreign-alloc :pointer :count 3)))
    (setf
     ;; The elements
     (cffi:mem-aref complex :pointer 0) +pointer-type-double+
     (cffi:mem-aref complex :pointer 1) +pointer-type-double+
     (cffi:mem-aref complex :pointer 2) (cffi:null-pointer)
     ;; The ffi-type
     (cffi:foreign-slot-value ptr 'ffi-type 'size) 0
     (cffi:foreign-slot-value ptr 'ffi-type 'alignment) 0
     (cffi:foreign-slot-value ptr 'ffi-type 'type) +type-struct+
     (cffi:foreign-slot-value ptr 'ffi-type 'elements) complex)
    ptr))

(defvar test-prep
  (with-foreign-objects
      ((cif 'ffi-cif)
       (argtypes :pointer 1)
       ;(arguments :pointer 1)
       ;(result :double)
       )
    (setf (cffi:mem-aref argtypes :pointer 0) +pointer-type-complex+)
    (when (eql
	   :OK
	   (prep-cif cif :default-abi 1 +pointer-type-double+ argtypes))
      t)))

#|
    (call cif
	  (foreign-symbol-pointer "gsl_complex_abs")
	  result
	  )
|#
